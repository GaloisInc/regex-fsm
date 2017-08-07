module Regex.Parse where

import Control.Monad
import Regex.ENFA

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

term :: Parser (Reg Char)
term = buildExpressionParser ops atom where
  ops = [ [ Postfix (Rep <$ char '*') ]
        , [ Infix (return Cat) AssocRight ]
        , [ Infix (Union <$ char '|') AssocRight ]
        ]
  atom = msum
    [ Lit <$> noneOf "*|()"
    , between (char '(') (char ')') term
    ]

main :: IO ()
main = parseTest term "he(llo)*|world"

