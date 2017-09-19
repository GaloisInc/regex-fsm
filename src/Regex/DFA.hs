{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , simulateDFA
  ) where

import           Control.Applicative
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           GHC.Generics
import           Control.DeepSeq

import           Math.Conjunction
import           Regex.Closure       (getClosure)
import           Regex.Secretive
import           Regex.Types         (ENFA(..))

subset ::
  (Ord a, Ord s) =>
  Set a ->
  ENFA s (Secretive a) ->
  DFA (Set s) (SecretiveTransition a)
subset alphabet enfa@ENFA{..} = DFA
  { start = dfaStart
  , trans = dfaTrans
  , finals = S.filter isFinal (M.keysSet dfaTrans)
  } where
  closureMap = getClosure enfa
  dfaStart = closureMap M.! start
  dfaTrans = M.fromList (dfs S.empty [dfaStart])
  isFinal = not . S.null . S.intersection final

  neighbors ss = deterministicEdges where
    nondeterministicEdges =
      [ (secret, S.unions . map (closureMap M.!) . S.toList $ ns)
      | s <- S.toList ss
      , (Just secret, ns) <- M.toList (M.findWithDefault M.empty s trans)
      ]
    deterministicEdges =
      [ (transition, S.unions
          [ neighbors
          | (secret, neighbors) <- nondeterministicEdges
          , transition `implies` secret
          ]
        )
      | transition <- dnf alphabet (map fst nondeterministicEdges)
      ]

  dfs seen [] = []
  dfs seen (node:nodes)
    | node `S.member` seen = dfs seen nodes
    | otherwise = (node, M.fromList ns)
                : dfs (S.insert node seen) (map snd ns ++ nodes)
                where ns = neighbors node

-- | DFA
data DFA s a
  = DFA
  { trans :: Map s (Map a s)
    -- ^ Transitions in the DFA
  , start :: s
    -- ^ Initial starting state
  , finals :: Set s
    -- ^ Final states
  } deriving (Show, Eq, Generic)

instance NFData (DFA (Set Int) Char)

-- | Minimize a DFA
-- Two DFAs are called equivalent if they recognize the same regular language.
-- For every regular language L, there exists a unique, minimal DFA that recognizes L
minimize :: (Ord a, Ord s) => DFA s (SecretiveTransition a) -> DFA s (SecretiveTransition a)
minimize dfa@DFA {..} =
  DFA { trans = update
      , start = rewrite start
      , finals = S.map rewrite finals
      }
  where
    update = id
      . fmap (fmap rewrite)
      . M.mapKeys rewrite
      $ trans
    rewriteRules = equivalentToRewrite . equivalentStates $ dfa
    rewrite s = M.findWithDefault s s rewriteRules

smallPairs :: [t] -> [(t, t)]
smallPairs xs = do
  r : rs <- tails xs
  x' <- rs
  return (r, x')

related :: Ord s => Set (s,s) -> s -> s -> Bool
related rel s s' =
  s == s' || (s, s') `S.member` rel || (s', s) `S.member` rel

initialize :: Ord s => DFA s a -> Set (s, s)
initialize DFA {..} = S.fromList finals' <> S.fromList nonFinals
   where
     finals' = smallPairs (S.toList finals)
     nonFinals =
         smallPairs
       . S.toList
       . (`S.difference` finals)
       . M.keysSet
       $ trans

step :: (Ord a, Ord s)
     => DFA s (SecretiveTransition a)
     -> Set (s, s)
     -> Set (s, s)
step dfa rel = S.filter (uncurry (oneStepEquivalent dfa rel)) rel

equivalentStates :: (Ord a, Ord s) => DFA s (SecretiveTransition a) -> Set (s, s)
equivalentStates dfa = go (initialize dfa)
  where
    go rel | rel' == rel = rel
           | otherwise = go rel'
      where
        rel' = step dfa rel

equivalentToRewrite :: Ord s => Set (s, s) -> Map s s
equivalentToRewrite s = M.fromListWith max
  [ (src, trgt)
  | (l,r) <- S.toList s
  , let [src,trgt] = sort [l,r]
  ]

simulateDFA
  :: Ord s
  => Ord a
  => [a]
  -> DFA s a -> Bool
simulateDFA xs' DFA {..} = go xs' start
  where
    go [] s = s `S.member` finals
    go (x:xs) s =
      case M.lookup s trans >>= M.lookup x of
        Nothing -> False
        Just set' -> go xs set'

-- Given an alphabet, a DFA, and a set of assumed equivalences, look for
-- evidence that a given pair of states should no longer be considered
-- equivalent.
oneStepEquivalent :: (Eq a, Ord s) => DFA s (SecretiveTransition a) -> Set (s, s) -> s -> s -> Bool
oneStepEquivalent DFA{..} equivalences l r = and $ do
  (stl, tgtl) <- M.assocs $ trans M.! l
  (str, tgtr) <- M.assocs $ trans M.! r
  return $  related equivalences tgtl tgtr
         -- notionally, we are asking whether (stl && str) is unsatisfiable
         || fromMaybe False (liftA2 (/=) (exactMatch stl) (exactMatch str))
         || (positive (conditions stl) `intersects` negative (conditions str))
         || (positive (conditions str) `intersects` negative (conditions stl))
  where
  intersects l r = not . S.null $ S.intersection l r
