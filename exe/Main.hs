{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import Control.Monad
import Options.Generic
import Text.ParserCombinators.Parsec
import Text.Show.Pretty
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

import Regex

-- | Example `./regex-fsm --regex (a|b)|(a|b) --inputLength 5 --verbose True
data Options
  = Options
  { regex :: String
  , inputLength :: Int
  , verbose :: Maybe Bool
  , output :: String
  } deriving (Show, Eq, Generic)

modifiers :: Modifiers
modifiers = defaultModifiers { shortNameModifier = firstLetter }

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers

-- | Main function
main :: IO ()
main = do
  Options {..} <- getRecord "regex-fsm"
  case parseRegex regex :: Either ParseError (Reg Char) of
    Left err -> do
      putStrLn "Failed to parse, error:"
      print err
    Right regex -> do
      let json =
           encode $ Matrices inputLength
                  $ toMatrices inputLength . minimize . subset . thompsons
                  $ regex
      BL.writeFile output json
      when (verbose == Just True) $ do
        putStrLn "== Regular Expression AST =="
        pPrint regex
        putStrLn "== Thompson's construction =="
        pPrint . thompsons $ regex
        putStrLn "== Closure construction =="
        pPrint . getClosure . thompsons $ regex
        putStrLn "== Subset construction =="
        pPrint . subset . thompsons $ regex
        putStrLn "== Minimized DFA =="
        pPrint . minimize . subset . thompsons $ regex
        putStrLn "== Matrix Branching Program =="
        pPrint . toMatrices inputLength . minimize . subset . thompsons $ regex
