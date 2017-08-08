{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Graph.Inductive.Dot
import Options.Generic
import Text.ParserCombinators.Parsec
import Text.Show.Pretty

import Regex.Types
import Regex.Parse
import Regex.ENFA
import Regex.DFA
import Regex.MBP

-- | Example `./regex-fsm --regex (a|b)|(a|b) --input ab
data Options
  = Options
  { regex :: String
  , input :: String
  } deriving (Show, Eq, Generic)

modifiers :: Modifiers
modifiers = defaultModifiers { shortNameModifier = firstLetter }

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers

-- | Main function
main :: IO ()
main = do
  Options {..} <- getRecord "regex-fsm"
  case parse regexParser "regex" regex :: Either ParseError (Reg Char) of
    Left err -> do
      putStrLn "Failed to parse, error:"
      print err
    Right regex -> do
      putStrLn "regex"
      pPrint regex
      putStrLn "thompons"
      pPrint . thompsons $ regex
      putStrLn "subset"
      pPrint . subset . thompsons $ regex
      putStrLn "minimized"
      pPrint . minimize . subset . thompsons $ regex
      putStrLn "matrices"
      pPrint . toMatrices input . minimize . subset . thompsons $ regex
