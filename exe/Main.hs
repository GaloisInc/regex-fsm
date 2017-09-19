{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import           Control.Monad
import           Data.Aeson                    hiding (json)
import qualified Data.ByteString.Lazy          as BL
import           Options.Generic
import           Text.ParserCombinators.Parsec
import           Text.Show.Pretty

import           Regex

import qualified Data.Set as S

-- | Example `./regex-fsm --regex (a|b)|(a|b) --inputLength 5 --verbose True
data Options
  = Options
  { regex :: String
  , inputLength :: Int
  , verbose :: Maybe Bool
  , output :: String
  , alphabet :: String
  } deriving (Show, Eq, Generic)

modifiers :: Modifiers
modifiers = defaultModifiers { shortNameModifier = firstLetter }

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers

-- | Main function
main :: IO ()
main = do
  Options {..} <- getRecord "regex-fsm"
  case parseRegex regex :: Either ParseError (Reg (Secretive Char)) of
    Left err -> do
      putStrLn "Failed to parse, error:"
      print err
    Right regex' -> do
      let thompson :: ENFA Int (Secretive Char)
          thompson  = thompsons regex'
          closure   = getClosure thompson
          subset'   = subset (S.fromList alphabet) thompson
          numbered  = renumber subset'
          minimized = minimize numbered
          -- TODO: symbolic matrices, too!
          -- matrices  = toMatrices inputLength minimized
      -- BL.writeFile output $ encode (Matrices inputLength matrices)
      when (verbose == Just True) $ do
        putStrLn "== Regular Expression AST =="
        pPrint regex'
        putStrLn "== Thompson's construction =="
        pPrint thompson
        putStrLn "== Closure construction =="
        pPrint closure
        putStrLn "== Subset construction =="
        pPrint subset'
        putStrLn "== Int-based DFA =="
        pPrint numbered
        putStrLn "== Minimized DFA =="
        pPrint minimized
        -- putStrLn "== Matrix Branching Program =="
        -- pPrint matrices
