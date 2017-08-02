{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Regex.ENFA
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <david@galois.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Regex.ENFA
  ( -- * Types re-exported
    Reg  (..)
  , ENFA (..)
  , Transition
    -- * ENFA smart constructors
  , (-->)
  , (#)
  , (|->)
  , (-|)
    -- * Thompson's construction
  , thompsons
  ) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S

import           Regex.Types

-- | Tuple smart constructor
(-|) :: a -> b -> (a,b)
(-|) = (,)

-- | Epsilon `Transition` smart constructor
(|->) :: (s,a) -> s -> Transition s a

-- | Transition smart constructor
(x,y) |-> s = M.singleton x $ M.singleton (Just y) (S.singleton s)

-- | Union Transition
(#) :: (Ord s, Ord a) => Transition s a -> Transition s a -> Transition s a
(#) = M.unionWith (M.unionWith (S.union))
infixr 5 #

-- | Single Transition construction
(-->) :: k -> a1 -> M.Map k (M.Map (Maybe a) (Set a1))
s --> s' = M.singleton s (M.singleton Nothing (S.singleton s'))

-- | Thompson's construction
thompsons :: (Ord s, Num s, Ord a) => Reg a -> ENFA s a
thompsons = flip evalState 0 . go
  where
    -- | "a|b"
    go (a' `Union` b') = do
      (a, b) <- liftM2 (,) (go a') (go b')
      n <- newState
      pure ENFA {
        start = n
      , states = states a `S.union` states b `S.union` S.singleton n
      , final = mconcat [ final a, final b ]
      , trans =
          trans a # trans b
                  # n --> start a
                  # n --> start b
      }
    -- | "ab"
    go (a' `Cat` b') = do
      a <- go a'
      b <- go b'
      pure ENFA {
        start = start a
      , states = states a `S.union` states b
      , final = final b
      , trans = foldr (go' b) (trans a # trans b) (final a)
      } where
          go' b state trans =
            state --> start b # trans
    -- | "a*"
    go (Rep a') = do
      a <- go a'
      pure a {
        trans = foldr (go' a) (trans a) (final a)
      , final = S.singleton (start a)
      }
        where
          go' b state trans =
            state --> start b # trans
    -- | "a"
    go (Lit x) = do
      start' <- newState
      final' <- newState
      pure ENFA {
        start = start'
      , states = S.fromList [start',final']
      , trans = start' -| x |-> final'
      , final = S.singleton final'
      }
    -- | ""
    go Eps = do
      n <- newState
      pure $ ENFA mempty n (S.singleton n) (S.singleton n)

    -- Requests new state
    newState :: Num s => State s s
    newState = modify (+1) >> get
