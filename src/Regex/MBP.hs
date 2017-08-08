{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
module Regex.MBP ( toMatrices ) where

import           Control.Monad.State
import           Data.Map
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Set
import qualified Data.Set            as S
import           Regex.DFA

type Matrix = [[Int]]

buildStep :: Ord s => Ord a => DFA s a -> Set s -> (Map a Matrix, Set s)
buildStep DFA {..} srcs = (matrices, tgts)
  where
    alphabet = Prelude.map snd (M.keys trans)
    matrices =
      M.fromList [
         (a, matrixFor a)
        | a <- alphabet
        ]
    matrixFor a = [
        [ if M.lookup (src,a) trans == Just tgt
            then 1
            else 0
        | src <- S.toAscList srcs
        ]
      | tgt <- S.toAscList tgts
      ]
    tgts =
      S.fromList . catMaybes $
        [ x
        | s <- S.toList srcs
        , a <- alphabet
        , let x = M.lookup (s, a) trans
        ]

-- | Convert to Matrices
toMatrices
  :: (Ord a, Ord s, Foldable t)
  => t a
  -> DFA s a
  -> [Map a Matrix]
toMatrices input dfa@DFA{..}
  = flip evalState (S.singleton start)
  $ replicateM (length input)
  $ state (buildStep dfa)
