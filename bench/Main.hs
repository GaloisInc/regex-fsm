{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.IO.Encoding
import Criterion.Main
import Data.Map
import Data.Matrix
import Data.Set
import Regex

parse :: String -> Reg Char
parse r =
  case parseRegex r of
    Right x -> x
    Left _ -> error "Couldn't parse"

constructThompsons :: String -> ENFA Integer Char
constructThompsons = thompsons . parse

constructSubset :: String -> DFA (Set Int) Char
constructSubset = subset . thompsons . parse

constructMinimized :: String -> DFA (Set Integer) Char
constructMinimized = minimize . subset . thompsons . parse

constructMatrix
  :: Int
  -> String
  -> [Map Char (Matrix Int)]
constructMatrix n =
    toMatrices' n
  . minimize
  . subset
  . thompsons
  . parse
    where
      toMatrices' :: Int -> DFA (Set Int) Char -> [Map Char (Matrix Int)]
      toMatrices' = toMatrices

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain [
    bgroup "Parsing"
      [ bench "parse 01110101000010010110011010000011"
          (whnf parse "01110101000010010110011010000011")
    , bench "parse 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
        (whnf parse "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)")
    , bench "parse (0|1)*0101010000110000(0|1)*"
        (whnf parse "(0|1)*0101010000110000(0|1)*")
    ]
   , bgroup "Thompson's construction"
    [ bench "thompon's 01110101000010010110011010000011"
        (whnf constructThompsons "01110101000010010110011010000011")
    , bench "thompson's 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
        (whnf constructThompsons "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)")
    , bench "thompson's (0|1)*0101010000110000(0|1)*"
        (whnf constructThompsons "(0|1)*0101010000110000(0|1)*")
    ]
   , bgroup "Subset construction"
    [ bench "subset 01110101000010010110011010000011"
        (whnf constructSubset "01110101000010010110011010000011")
    , bench "subset 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
        (whnf constructSubset "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)")
    , bench "subset (0|1)*0101010000110000(0|1)*"
        (whnf constructSubset "(0|1)*0101010000110000(0|1)*")
    ]
   , bgroup "Minimized construction"
    [ bench "minimized 01110101000010010110011010000011"
        (whnf constructMinimized "01110101000010010110011010000011")
    , bench "minimized 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
        (whnf constructMinimized "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)")
    , bench "minimized (0|1)*0101010000110000(0|1)*"
        (whnf constructMinimized "(0|1)*0101010000110000(0|1)*")
    ]
   , bgroup "Matrix construction"
    [ bench "matrix 01110101000010010110011010000011"
        (let l = "01110101000010010110011010000011" :: String
             n = length l
         in whnf (constructMatrix n) l)
    , bench "matrix 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
        (let l = "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)" :: String
             n = length l
         in whnf (constructMatrix n) l)
    , bench "matrix (0|1)*0101010000110000(0|1)*"
        (let l = "(0|1)*0101010000110000(0|1)*" :: String
             n = length l
         in whnf (constructMatrix n) l)
    ]
   ]
