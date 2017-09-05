{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import qualified Data.Map             as M
import           Data.Set
import           System.Process
import           System.Random

import           Regex

pointFunctions :: IO [String]
pointFunctions = forM [32,36..64] point

infixPointFunctions :: IO [String]
infixPointFunctions = forM [32,36..64] infixPoint

conjunctionFunctions :: IO [String]
conjunctionFunctions = forM [32,36..64] conjunctions

conjunctions :: Int -> IO String
conjunctions n =
  concatMap go <$> do
    replicateM n $ randomRIO (0 :: Int, 2)
    where
      go 0 = "0"
      go 1 = "1"
      go 2 = "(0|1)"
      go _ = error "impossible"

infixPoint :: Int -> IO String
infixPoint n = go <$> point (n - 16)
  where
    go s = "(0|1)*" ++ s ++ "(0|1)*"

point :: Int -> IO [Char]
point n = do
  concatMap show <$> do
    replicateM n $ randomRIO (0 :: Int, 1)

randomInput :: Int -> IO [Char]
randomInput = point

incr :: IORef Int -> IO Int
incr ref = modifyIORef ref (+1) >> readIORef ref

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  putStrLn "Point function tests"
  pfs <- pointFunctions
  forM_ pfs $ \pf -> do
    n <- incr ref
    putStrLn $ "Test number: " ++ show n
    putStrLn $ "(Regex, Input): " ++ show (pf, pf)
    testObfuscatorWithSecurity pf pf (length pf) 40

  putStrLn "Conjunction function tests"
  cfs <- conjunctionFunctions
  forM_ (zip [32,36..64] cfs) $ \(k,cf) -> do
    n <- incr ref
    xs <- point k
    putStrLn $ "Test number: " ++ show n
    putStrLn $ "(Regex, Input): " ++ show (cf, xs)
    testObfuscatorWithSecurity cf xs (length xs) 40

  putStrLn "Infix function tests"
  ipfs <- infixPointFunctions
  forM_ (zip [32,36..64] ipfs) $ \(i,cf) -> do
    n <- incr ref
    xs' <- point i
    putStrLn $ "Test number: " ++ show n
    putStrLn $ "(Regex, Input): " ++ show (cf, xs')
    testObfuscatorWithSecurity cf xs' (length xs') 40

type RegexString = String
type ProcessArg = String

type SecParam = Int
type Length = Int

testObfuscatorWithSecurity
  :: RegexString
  -> ProcessArg
  -> Length
  -> SecParam
  -> IO ()
testObfuscatorWithSecurity str arg n secParam = do
  let Right regex = parseRegex str
      dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
      test = Matrices n $ M.mapKeys pure <$> toMatrices n dfa
      fileName = "output.json"
  BL.writeFile fileName (encode test)
  putStrLn "Obfuscating"
  e' <- readProcess "./result/bin/obfuscator"
    [ "obf"
    , "--load"
    , fileName
    , "--secparam"
    , show secParam
    , "--verbose"
    ] []
  mapM_ print (lines e')
  putStrLn "Evaluating"
  e <- readProcess "./result/bin/obfuscator"
    [ "obf"
    , "--load-obf"
    , fileName ++ ".obf." ++ show secParam
    , "--eval"
    , arg
    , "--verbose"
    ] []
  mapM_ print (lines e)
  void $ system "rm -r output.json.obf*"
