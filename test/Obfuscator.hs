{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Set
import           Options.Generic
import           System.IO
import           System.Process
import           System.Random

import           Regex

pointFunctions :: IO [String]
pointFunctions = forM [32,36..72] point

infixPointFunctions :: IO [String]
infixPointFunctions = forM [32,36..72] infixPoint

conjunctionFunctions :: IO [String]
conjunctionFunctions = forM [32,36..72] conjunctions

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

-- | Example `./obfuscator-tests --secParam 40
data Options
  = Options
  { runTestSuites :: Bool
  , secParam :: Maybe Int
  , pointTest :: Bool
  , infixPointTest :: Bool
  , conjunctionTest :: Bool
  , folderPath :: Maybe String
  } deriving (Show, Eq, Generic)

modifiers :: Modifiers
modifiers = defaultModifiers { shortNameModifier = firstLetter }

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers

main :: IO ()
main = do
  Options {..} <- getRecord "obfuscator-tests"
  let secParam' = fromMaybe 80 secParam
      path' = fromMaybe "" folderPath
  case runTestSuites of
    True -> runSuite path'
    False -> do
      putStrLn $ "Security Parameter of: " ++ show secParam'
      when pointTest $ do
        putStrLn "Running Point tests.."
      when conjunctionTest $ do
        putStrLn "Running Conjunction tests..."
      when infixPointTest $ do
        putStrLn "Running Infix Point tests.."
      putStrLn $ "Folder path: " <> path'
      let opts = defaultOpts {
          pointTest' = pointTest
        , infixPointTest' = infixPointTest
        , conjunctionTest' = conjunctionTest
        }
      oneShot opts path' secParam'

-- | Test options
data TestOpts
  = TestOpts
  { pointTest' :: Bool
  , infixPointTest' :: Bool
  , conjunctionTest' :: Bool
  } deriving (Show, Eq)

defaultOpts :: TestOpts
defaultOpts = TestOpts True True True

runSuite :: String -> IO ()
runSuite path = forM_ [40, 80] (oneShot defaultOpts path)

oneShot :: TestOpts -> String -> SecParam -> IO ()
oneShot TestOpts {..} path s = do
  ref <- newIORef (0 :: Int)
  when pointTest' (pointTests ref s path)
  when conjunctionTest' (conjunctionTests ref s path)
  when infixPointTest' (infixTests ref s path)

pointTests :: IORef Int -> SecParam -> String -> IO ()
pointTests ref s path = do
  hPutStrLn stderr "Point function tests"
  pfs <- pointFunctions
  forM_ pfs $ \pf -> do
    n <- incr ref
    hPutStrLn stderr $ "Test number: " ++ show n
    hPutStrLn stderr $ "(Regex, Input): " ++ show (pf, pf)
    testObfuscatorWithSecurity path pf pf (length pf) s 1

conjunctionTests :: IORef Int -> SecParam -> String -> IO ()
conjunctionTests ref s path = do
  hPutStrLn stderr "Conjunction function tests"
  cfs <- conjunctionFunctions
  forM_ (zip [32,36..72] cfs) $ \(k,cf) -> do
    n <- incr ref
    xs <- point k
    hPutStrLn stderr $ "Test number: " ++ show n
    hPutStrLn stderr $ "(Regex, Input): " ++ show (cf, xs)
    testObfuscatorWithSecurity path cf xs (length xs) s 1

infixTests :: IORef Int -> SecParam -> String ->IO ()
infixTests ref s path = do
  hPutStrLn stderr "Infix function tests"
  ipfs <- infixPointFunctions
  forM_ (zip [32,36..72] ipfs) $ \(i,cf) -> do
    n <- incr ref
    xs' <- point i
    hPutStrLn stderr $ "Test number: " ++ show n
    hPutStrLn stderr $ "(Regex, Input): " ++ show (cf, xs')
    testObfuscatorWithSecurity path cf xs' (length xs') s 1

type RegexString = String
type ProcessArg = String

type SecParam = Int
type Length = Int

alphabet :: String
alphabet = "01"

testObfuscatorWithSecurity
  :: String
  -> RegexString
  -> ProcessArg
  -> Length
  -> SecParam
  -> Int
  -> IO ()
testObfuscatorWithSecurity path str arg n secParam chunks = do
  let Right regex = parseRegex str
      dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
      test = Matrices n $ premultiply chunks (toMatrices n dfa)
      fileName =
        if Prelude.null path
          then "output.json"
          else path ++ "/output.json"
  BL.writeFile fileName (encode test)
  hPutStrLn stderr "Obfuscating"
  e' <- readProcess "./result/bin/obfuscator"
    [ "obf"
    , "--load"
    , fileName
    , "--secparam"
    , show secParam
    , "--verbose"
    ] []
  mapM_ (hPutStrLn stderr) (lines e')
  hPutStrLn stderr "Evaluating"
  e <- readProcess "./result/bin/obfuscator"
    [ "obf"
    , "--load-obf"
    , fileName ++ ".obf." ++ show secParam
    , "--eval"
    , arg
    , "--verbose"
    ] []
  mapM_ (hPutStrLn stderr) (lines e)
  void $ system "rm -r output.json.obf*"
