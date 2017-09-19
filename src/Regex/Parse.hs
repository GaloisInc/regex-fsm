{-# LANGUAGE OverloadedStrings #-}
module Regex.Parse ( parseRegex ) where

import Control.Monad
import Regex.ENFA
import Regex.Secretive

import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

-- | Helper function to parse a regular expression
parseRegex :: String -> Either ParseError (Reg (Secretive Char))
parseRegex regex = runParser regexParser 0 "regex" regex

-- | Regex parser
regexParser :: Parsec String Int (Reg (Secretive Char))
regexParser = buildExpressionParser ops atom where
  ops = [ pure $ Postfix (Rep <$ char '*')
        , pure $ Infix (return Cat) AssocRight
        , pure $ Infix (Union <$ char '|') AssocRight
        ]
  atom = msum
    [ Lit . Public <$> noneOf "*|()x"
    , between (char '(') (char ')') regexParser
    , do
      _ <- char 'x'
      var <- getState
      setState (var+1)
      return . Lit . Secret $ var
    ]

