{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Matrix
import Data.Text (pack)
import Regex.Types
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

sequentialPositions :: [Step] -> Bool
sequentialPositions steps =
  map position steps == zipWith (const . pack . show) [0 :: Int ..] steps

sanityCheckAlphabet :: [Step] -> IO ()
sanityCheckAlphabet steps = case map (M.keysSet . branches) steps of
  [] -> die "No steps!"
  s:ss -> id
       . mapM_ warn
       . filter ((/= s) . snd)
       . zip [1..]
       $ ss
    where
      warn (i :: Int, s') = putStrLn . unlines $ [
          "WARNING: Alphabet " ++ show s'
        , " at position " ++ show i
        , " does not match alphabet " ++ show s
        , " at position 0."
        ]

maxSize :: [Step] -> Int
maxSize steps = maximum
  [ dim
  | Step { sDims = (r, c) } <- steps
  , dim <- [r, c]
  ]

class ZeroPad a where zeroPad :: Int -> Int -> a -> a

instance Num a => ZeroPad (Matrix a) where
  -- special case: can't construct a zero-row matrix for padBottom
  zeroPad r c m = (m <|> padRight) <-> padBottom where
    padRight  = zero (nrows m) (c - ncols m)
    padBottom = zero (r - nrows m) c

instance ZeroPad v => ZeroPad (Map k v) where
  zeroPad r c = fmap (zeroPad r c)

instance ZeroPad Step where
  zeroPad r c (Step _ pos bs) = Step (r,c) pos (zeroPad r c bs)

instance ZeroPad a => ZeroPad [a] where
  zeroPad r c = map (zeroPad r c)

pprint :: [Step] -> String
pprint         = bracket . concatMap pprintStep where
  pprintStep   = bracket . concatMap pprintBranch . M.toAscList . branches
  pprintBranch = bracket . concatMap pprintRow . toLists . snd
  pprintRow    = bracket . unwords . map show
  bracket s    = "[" ++ s ++ "]"

convert :: ByteString -> IO ()
convert bs = case eitherDecode bs of
  Right (MBPInterpreter steps _) -> do
    unless (sequentialPositions steps) $
      putStrLn "WARNING: Ignoring non-sequential position information."
    sanityCheckAlphabet steps
    let size = maxSize steps
    writeFile ("n" ++ show size ++ "_L" ++ show (length steps) ++ ".txt")
              (pprint . zeroPad size size $ steps)
  Left err -> die $ "Could not read matrix branching program:\n" ++ err

usage :: IO String
usage = do
  progName <- getProgName
  return $ "USAGE: " ++ progName ++ " [--] FILE\n"
        ++ "   OR: " ++ progName ++ " -h\n"
        ++ "   OR: " ++ progName ++ " --help\n"
        ++ "\n"
        ++ "Convert a matrix branching program in JSON format to NTL format.\n"
        ++ "Use - to read from stdin. If you want to read from files named -, -h, or\n"
        ++ "--help, insert -- before the filename.\n"
        ++ "\n"
        ++ "This conversion is lossy:\n"
        ++ "* The information available in the JSON format about the meaning of the output\n"
        ++ "  of the matrix branching program is lost outright.\n"
        ++ "* The information about which input positions to inspect for each matrix is\n"
        ++ "  lost; a warning is printed if the JSON has instructions to read the input in\n"
        ++ "  any other order than front to back.\n"
        ++ "* The information about the alphabet is lost. Matrices in each step of the\n"
        ++ "  branching program are printed in Unicode codepoint order. A warning is printed\n"
        ++ "  if the alphabet varies from step to step.\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]       -> usage >>= putStrLn
    ["--help"]   -> usage >>= putStrLn
    ["-"]        -> LBS.getContents   >>= convert
    ["--", file] -> LBS.readFile file >>= convert
    [file]       -> LBS.readFile file >>= convert
    _            -> usage >>= die
