{-# LANGUAGE OverloadedStrings #-}
module Regex.Parse ( regexParser ) where

import Control.Monad
import Regex.ENFA

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

regexParser :: Parser (Reg Char)
regexParser = buildExpressionParser ops atom where
  ops = [ pure $ Postfix (Rep <$ char '*')
        , pure $ Infix (return Cat) AssocRight
        , pure $ Infix (Union <$ char '|') AssocRight
        ]
  atom = msum
    [ Lit <$> noneOf "*|()"
    , between (char '(') (char ')') regexParser
    ]

