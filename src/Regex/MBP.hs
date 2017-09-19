{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regex.MBP ( toMatrices, simulateMBP ) where

import           Control.Monad.State
import           Data.List
import           Data.Map
import qualified Data.Map            as M
import           Data.Matrix         as Matrix
import           Data.Maybe
import           Data.Set
import qualified Data.Set            as S
import           Regex.DFA

buildStep
  :: Ord s
  => Ord a
  => DFA s a
  -> Set s
  -> (Map a (Matrix Int), Set s)
buildStep DFA {..} srcs = (M.map fromLists matrices, tgts)
  where
    ordNub = fmap head . group . sort
    alphabet = ordNub $ M.elems trans >>= M.keys
    matrices = M.fromList [ (a, matrixFor a) | a <- alphabet ]
    matrixFor a = [
        [ if (M.lookup src trans >>= M.lookup a) == Just tgt
            then 1
            else 0
        | tgt <- S.toAscList tgts
        ]
      | src <- S.toAscList srcs
      ]
    tgts =
      S.fromList . catMaybes $
        [ x
        | s <- S.toList srcs
        , a <- alphabet
        , let x = M.lookup s trans >>= M.lookup a
        ]

-- | Convert to Matrices
toMatrices
  :: (Ord a, Ord s)
  => Int
  -> DFA s a
  -> [ Map a (Matrix Int) ]
toMatrices inputLength dfa@DFA{..} = updatedMatrices
  where
    updatedMatrices
      | Prelude.null matrices = []
      | otherwise = init matrices ++ [ M.map (`multStd` bookend) (last matrices) ]
    bookend = constructBookend srcs finals
    (matrices, srcs) =
      flip runState (S.singleton start)
        $ replicateM inputLength
        $ state (buildStep dfa)

    constructBookend
      :: Ord s
      => Set s
      -> Set s
      -> Matrix Int
    constructBookend sources finalStates = result
     where
       result = fromLists
        [ x
        | s <- S.toAscList sources
        , let x = if s `S.member` finalStates
                    then [0]
                    else [1]
        ]

simulateMBP :: Ord a => [a] -> [ Map a (Matrix Int) ] -> Bool
simulateMBP input matrices = zeroTest && validate
  where
    result = zipWith M.lookup input matrices
    validate =
      if length result /= length input
        then False
        else True
    zeroTest =
      case catMaybes result of
        [] -> False
        xs -> case Matrix.toList $ foldl1 multStd xs of
            [0] -> True
            _   -> False
