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
import Regex.Closure
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
      putStrLn "== Regular Expression AST =="
      pPrint regex
      putStrLn "== Thompon's construction =="
      pPrint . thompsons $ regex
      putStrLn "== Closure construction =="
      pPrint . getClosure . thompsons $ regex
      putStrLn "== Subset construction =="
      pPrint . subset . thompsons $ regex
      putStrLn "== Minimized DFA =="
      pPrint . minimize . subset . thompsons $ regex
      putStrLn "== Matrix Branching Program =="
      pPrint . toMatrices input . minimize . subset . thompsons $ regex
