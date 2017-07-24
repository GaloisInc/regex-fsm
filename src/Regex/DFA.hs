{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Regex.DFA
  ( -- * Types
    DFA  (..)
  ) where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Regex.ENFA
import           Regex.NFA           hiding ( getTransitions
                                            , DFSState(..)
                                            , emptyState
                                            , popStack
                                            , pushStack
                                            , hasVisited
                                            )
-- | DFA
data DFA s a
  = DFA { trans :: Map (Set s) (Map a (Set s))
        -- ^ Transitions in the DFA
        , start :: Set s
        -- ^ Initial starting state
        , finals :: Set (Set s)
        -- ^ Final states
        } deriving (Show, Eq)

-- type DFATrans s a = Map (Set s) (Map a (Set s))

-- | DFA
emptyDFA :: Ord s => s -> DFA s a
emptyDFA start' = DFA mempty (S.singleton start') mempty

-- test' :: Reg Char
-- test' =
--   (Rep ((Lit 'a') `Union` (Lit 'b') `Union` (Lit 'c'))) `Cat` (Lit 'a')

-- foo :: ENFA Integer Char
-- foo = toENFA test'

-- showFoo = do
--   print (Regex.ENFA.start foo)
--   mapM_ print . M.assocs $ Regex.ENFA.trans foo
--   mapM_ print (Regex.ENFA.final foo)

-- | For all the transitions from the start node to its neighbors
-- If any edges have the same labels, insert them into the same Set in the DFA

-- | Finally, for all states in the DFA, if any of them were final states in the NFA,
-- they should also be marked as final in the DFA
data DFSState s a = DFSState {
    toVisit :: [s]
  , visited :: [s]
  , dfa :: DFATrans s a
  } deriving (Show, Eq)

emptyState :: (Ord s, Ord a) => DFSState s a
emptyState = DFSState mempty mempty mempty

-- | Convert NFA to DFA
-- toDFA :: (Ord s, Ord a) => NFA s a -> DFA s a
-- toDFA nfa@NFA{..} = dfa $ execState go (emptyState start)
--   where
--     setTrans = flip getTransitions trans
--     go = fix $ \loop -> do
--       maybeS <- popStack
--       forM_ maybeS $ \state -> do
--         visited <- hasVisited state
--         unless visited $ do
--           let transitions = getTransitions state trans
--           forM_ transitions $ \(a,f) ->
--             pushStack state
          -- forM_ dfat $ \set -> do
          --   pushStack set

-- Iterate over states in NFA


-- dfaTransitions
--   :: (Ord a, Ord s)
--   => Set s
--   -> NFA s a
--   -> Map a (Set s)
-- dfaTransitions s' NFA{..} = foldr go mempty []
--   where
--     go (a,s) = M.insertWith S.union a s
--     ts = getTransitions s' trans

-- | Retrieve transitions
getTransitions :: (Ord s, Ord a) => s -> Trans' s a -> S.Set (a, s)
getTransitions e trans = fromMaybe mempty $ do
   transitionMap <- M.lookup e trans
   pure . S.fromList $ concat
    [ ys
    | (a, s) <- M.assocs transitionMap
    , let ys = zip (repeat a) (S.toList s)
    ]

hasVisited
  :: (MonadState (DFSState s a) f, Eq s)
  => s
  -> f Bool
hasVisited s = elem s <$> gets visited

markVisited
  :: MonadState (DFSState s a) m
  => s
  -> m ()
markVisited e =
  modify $ \s -> s { visited = e : visited s }

popStack :: State (DFSState s a) (Maybe s)
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
pushStack pos = do
  modify $ \ns -> ns {
    toVisit = pos : toVisit ns
  }

addDFAState s = modify $ \n ->
  n { dfa = undefined }

type DFATrans s a = Set (s, a, s)

-- | For all states in the ENFA that have an epsilon closure
-- , if any state in the epsilon closure contain transitions to other nodes
-- that are not epsilon transitions, make a transition in the NFA from the original state to that node.

-- | Traverse the list of nodes in the ENFA
-- Look up the list of nodes in the epsilon closure
-- For all the nodes in the epsilon closure, lookup in the ENFA transition list
-- to see if there are any moves (not epsilon moves) that go directly
-- to others. If so, make a transition in the NFA.


-- ===============================

-- For all states in the epsilon closure of NFA start.
-- For all transitions of states in the epsilon closure that
-- are not in the epsilon closure. Check if a label exists already in the mapping of DFA states and epsilon closures.
-- If so, create a transition in the DFA state to that DFA state
-- otherwise, create one.

toDFA' :: (Ord s, Ord a) => ENFA s a -> DFATrans s a
toDFA' enfa@ENFA {..} = dfa $ execState go emptyState {
      toVisit = [start]
    } where
    closureMap = getClosure enfa
    go = fix $ \loop -> do
      firstNode <- popStack
      forM_ firstNode $ \s -> do
        forM_ (M.lookup s closureMap) $ \epsClosure -> do
          forM_ 
      loop
