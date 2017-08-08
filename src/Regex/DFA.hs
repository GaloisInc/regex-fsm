{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Regex.DFA
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <david@galois.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Regex.DFA
  ( -- * Types
    DFA  (..)
  , subset
  , minimize
  ) where

import           Control.Monad.State
import Debug.Trace
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S

import           Regex.Closure       hiding (DFSState(..), popStack, markVisited, hasVisited, pushStack, trans)
import           Regex.Types         hiding (DFSState(..), popStack, markVisited, hasVisited, pushStack, trans)

-- | Convert an ENFA into a DFA
subset :: Ord t => ENFA Integer t -> DFA (Set Integer) t
subset enfa@ENFA {..} = getDFA
  where
    -- Retrieve epsilon closure map (call DFS from every state in NFA)
    closureMap = getClosure enfa
    -- Retrieve alphabet from ENFA transition map
    alphabet = nub
             $ concatMap catMaybes
             $ map M.keys
             $ M.elems trans
    -- Push epsilon closure (our first DFA state) onto the stack
    startState = emptyState {
      toVisit = pure (closureMap M.! start)
    }
    -- Add DFA transition to transition map
    addDFATrans newState a nextState = modify $ \d@DFSState{..} -> d {
      transMap = M.insert (newState, a) nextState transMap
    }
    -- Take the DFA transitions accumulated, and actually construct a DFA
    constructDFA trans =
      DFA {
        -- DFA start state *is* e-closure
        start = closureMap M.! start
      , finals = S.filter (\set -> set `S.intersection` final /= S.empty)
               $ S.fromList (M.elems closureMap)
      , ..
      }
    -- Empty DFA state
    emptyState = DFSState mempty mempty mempty
    hasVisited s = elem s <$> gets visited
    markVisited e =
      modify $ \s -> s {
        visited = e : visited s
      }
    popStack = do
      ns <- get
      let (pos, newStack) = pop (toVisit ns)
      put ns { toVisit = newStack }
      return pos
        where
          pop :: [a] -> (Maybe a, [a])
          pop [] = (Nothing, [])
          pop (x:xs) = (Just x, xs)

    pushStack :: s -> State (DFSState s a) ()
    pushStack p = modify $ \ns -> ns { toVisit = p : toVisit ns }

    -- Visit DFA states as they are accumulated, careful to detect cycles
    getDFA = constructDFA . transMap . flip execState startState $ do
      fix $ \loop -> do
        -- Pop DFA state
        maybeState <- popStack
        forM_ maybeState $ \newState -> do
          visited <- hasVisited newState
          unless visited $ do
            -- Check for cycle (by marking as visited)
            markVisited newState
            -- Iterate over alphabet
            forM_ alphabet $ \a -> do
              nextState <- S.unions <$> do
                -- Iterate over states in DFA state
                forM (S.toList newState) $ \e -> do
                  pure $ fromMaybe S.empty $ do
                    -- Lookup state transitions
                    transMap <- M.lookup e trans
                    -- Lookup transitions at this specific symbol
                    states <- M.lookup (Just a) transMap
                    -- Of states we can transition to (via non-epsilon transitions)
                    -- include all other states reachable in their epsilon closure.
                    pure $ S.unions
                         . S.toList
                         . S.map (fromMaybe S.empty . flip M.lookup closureMap)
                         $ states
              -- Record this state, visit it later
              addDFATrans newState a nextState
              pushStack newState
          loop

-- | DFS State
data DFSState s a = DFSState {
    toVisit  :: [s]
  , visited  :: [s]
  , transMap :: Map (s, a) (s)
  } deriving (Show, Eq)

-- | DFA
data DFA s a
  = DFA
  { trans :: Map (s, a) s
    -- ^ Transitions in the DFA
  , start :: s
    -- ^ Initial starting state
  , finals :: Set s
    -- ^ Final states
  } deriving (Show, Eq)

-- | Minimize a DFA
-- Two DFAs are called equivalent if they recognize the same regular language.
-- For every regular language L, there exists a unique, minimal DFA that recognizes L
minimize :: Show s => (Ord a, Ord s) => DFA (Set s) a -> DFA (Set s) a
minimize dfa@DFA {..} =
  DFA { trans = update dfa
      , start = rewrite start
      , finals = S.map rewrite finals
      }
  where
    update dfa@DFA {..} =
        M.fromList
      . map (\((s,a),s') -> ((rewrite s, a), rewrite s'))
      . M.toList
      $ trans
    rewrite s =
      fromMaybe s $ M.lookup s $
          equivalentToRewrite (equivalentStates dfa)

smallPairs :: [t] -> [(t, t)]
smallPairs xs = do
  r : rs <- tails xs
  x' <- rs
  return (r, x')

related :: Ord s => Set (s,s) -> Maybe s -> Maybe s -> Bool
related rel (Just s) (Just s') =
  s == s' || (s, s') `S.member` rel || (s', s) `S.member` rel
related _ _ _ = False

initialize :: Show s => Ord s => DFA (Set s) a -> Set (Set s, Set s)
initialize DFA {..} = S.fromList finals' <> S.fromList nonFinals
   where
     finals' = smallPairs (S.toList finals)
     nonFinals =
         smallPairs
       . S.toList
       . (`S.difference` finals)
       . S.map fst
       . M.keysSet
       $ trans

step :: (Ord a, Ord s)
     => DFA (Set s) a
     -> Set (Set s, Set s)
     -> Set (Set s, Set s)
step dfa rel =
  S.fromList [ (s,s')
             | (s,s') <- S.toList rel
             , a <- S.toList $ getAlphabet dfa
             , let l = M.lookup (s, a) (Regex.DFA.trans dfa)
             , let r = M.lookup (s',a) (Regex.DFA.trans dfa)
             , related rel l r
             ]

getAlphabet :: Ord a => DFA s a -> Set a
getAlphabet DFA {..} =
  S.fromList [ snd x | x <- M.keys trans ]

equivalentStates :: Show s => (Ord a, Ord s) => DFA (Set s) a -> Set (Set s, Set s)
equivalentStates dfa = go (initialize dfa)
  where
    go rel | rel' == rel = rel
           | otherwise = go rel'
      where
        rel' = step dfa rel

equivalentToRewrite :: Ord s => Set (Set s, Set s) -> Map (Set s) (Set s)
equivalentToRewrite s = M.fromListWith max
  [ (src, trgt)
  | (l,r) <- S.toList s
  , let [src,trgt] = sort [l,r]
  ]
