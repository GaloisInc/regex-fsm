{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regex.MBP ( toMatrices, simulateMBP ) where

import           Control.Monad.State
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
    alphabet = Prelude.map snd (M.keys trans)
    matrices = M.fromList [ (a, matrixFor a) | a <- alphabet ]
    matrixFor a = [
        [ if M.lookup (src,a) trans == Just tgt
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
        , let x = M.lookup (s, a) trans
        ]

-- | Convert to Matrices
toMatrices
  :: (Ord a, Ord s)
  => Int
  -> DFA s a
  -> [ Map a (Matrix Int) ]
toMatrices inputLength dfa@DFA{..} = updatedMatrices
  where
    updatedMatrices =
      init matrices ++ [ M.map (`multStd` bookend) (last matrices) ]
    bookend = constructBookend srcs finals
    (matrices, srcs) =
      flip runState (S.singleton start)
        $ replicateM inputLength
        $ state (buildStep dfa)

    alphabet = snd <$> M.keys trans

    finalMatrices =
      M.fromList $ do
        a <- alphabet
        pure (a, createFinalMatrix a)

    createFinalMatrix a = [
        [ if M.lookup (src, a) trans == Just tgt
            then 1
            else 0
        | tgt <- S.toAscList finals
        ]
      | src <- S.toAscList srcs
      ]
    constructBookend
      :: Ord s
      => Set s
      -> Set s
      -> Matrix Int
    constructBookend sources finalStates = bookend
     where
       bookend = fromLists
        [ x
        | s <- S.toAscList sources
        , let x = if s `S.member` finalStates
                    then [1,0]
                    else [0,1]
        ]

simulateMBP :: Ord a => [a] -> [ Map a (Matrix Int) ] -> Bool
simulateMBP input matrices = zeroTest
  where
    result =
      foldl1 multStd $
        catMaybes $
          zipWith M.lookup input matrices
    zeroTest =
      case Matrix.toList result of
        [1, 0] -> True
        _      -> False

-- Regex: (a*|b*)

-- final matrices look like:
-- "a"
-- ( 0 0 0 )
-- ( 1 0 0 )
-- ( 0 0 0 )

-- "b"
-- ( 0 0 0 )
-- ( 0 0 0 )
-- ( 0 0 1 )

--- bbbbb = ( 0 0 1 ) -- this should be a match
bbbbb = putStrLn $ prettyMatrix $
  fromLists [[0, 1]] `multStd`
  fromLists [[1,0,0],[0,0,1]] `multStd`
  fromLists [[1,0,0],[1,0,0],[0,0,1]] `multStd`
  fromLists [[1,0,0],[1,0,0],[0,0,1]] `multStd`
  fromLists [[1,0,0],[1,0,0],[0,0,1]] `multStd`
  fromLists [[0,0,0],[0,0,0],[0,0,1]] -- final state matrix

--- aaaaa ( 1 0 0 ) -- this should be a match
aaaaa = putStrLn $ prettyMatrix $
  fromLists [[1, 0]] `multStd`
  fromLists [[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[0,0,0],[1,0,0],[0,0,0]] -- final state matrix

--- baaaa ( 0 0 0 ) -- this should not be a match
baaaa = putStrLn $ prettyMatrix $
  fromLists [[0, 1]] `multStd`
  fromLists [[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[0,0,0],[1,0,0],[0,0,0]] -- final state matrix

--- aaaab ( 0 0 0 ) -- this should not be a match
aaaab = putStrLn $ prettyMatrix $
  fromLists [[0, 1]] `multStd`
  fromLists [[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[0,0,0],[0,0,0],[0,0,1]] -- final state matrix

--- aaaab ( 0 0 0 ) -- this should not be a match
aabab = putStrLn $ prettyMatrix $
  fromLists [[0, 1]] `multStd`
  fromLists [[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[1,0,0],[0,0,1]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[1,0,0],[0,1,0],[1,0,0]] `multStd`
  fromLists [[0,0,0],[0,0,0],[0,0,1]] -- final state matrix



