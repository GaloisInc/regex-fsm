module Main where

import Data.Graph.Inductive.Dot
import Options.Generic
import Regex.FSM

-- | OutputType (i.e. `-outputType json`)
data OutputType
  = JSON
  | DOT
  deriving (Show, Eq)

-- | Options `regex-fsm -o file -t dot -r a|b -i a`
-- Writes file.dot
data Options
  = Options { outputFileName :: String
            , outputType :: OutputType
            , regex :: String
            , input :: String
            } deriving (Show, Eq)

-- | Main function
main :: IO ()
main =
  writeFile "file.dot" $
    showDot (fglToDot enfa)
