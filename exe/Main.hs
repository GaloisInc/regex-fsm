{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Graph.Inductive.Dot
import Options.Generic
import Regex.ENFA

-- | OutputType (i.e. `-outputType json`)
data OutputType
  = JSON
  | DOT
  deriving (Show, Eq, Generic, Read)

instance ParseField OutputType
instance ParseFields OutputType
instance ParseRecord OutputType

-- | Options `regex-fsm -o file -t dot -r a|b -i a`
-- Writes file.dot
data Options
  = Options { outputFileName :: String
            , type' :: OutputType
            , regex :: String
            , input :: String
            } deriving (Show, Eq, Generic)

modifiers :: Modifiers
modifiers = defaultModifiers
  { shortNameModifier = firstLetter }

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers

showENFA enfa =
  writeFile "file.dot" $ showDot (fglToDot enfa)

-- | Main function
main :: IO ()
main = do
   options :: Options <- getRecord "regex-fsm"
   print options
   return ()
