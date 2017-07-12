module Regex.Parse where

import Control.Monad
import Regex.ENFA
import Text.ParserCombinators.Parsec

type RegexParser = Parser (Reg Char)

-- | Regular Expression parser
regex :: RegexParser
regex = msum [ try alt' ]
  where
    alt' = between (char '(') (char ')') (try alt <|> alt')

-- | Parse alternative
alt :: RegexParser
alt = do
  l <- try lit <|> regex
  char '|'
  r <- try lit <|> regex
  pure $ l `Union` r

-- | Consume at least one bit from a bitstring
lit :: RegexParser
lit = Lit <$> letter

-- | Epsilon Parser
eps :: RegexParser
eps = Eps <$ string mempty

-- | Union (Lit 'a') (Union (Lit 'b') (Union (Lit 'e') (Lit 'c')))
test :: IO ()
test = parseTest regex "(a|(b|(e|c)))"
