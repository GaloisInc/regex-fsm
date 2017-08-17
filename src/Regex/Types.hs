{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Regex.Types where

import           Control.Monad.State
import           Data.Aeson
import           Data.Data
import           Data.Default
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Matrix
import           Data.Monoid
import           Data.Set            (Set, insert)
import           Data.Text           (pack)

type Transition s a = Map s (Map (Maybe a) (Set s))
type EpsClosure s = M.Map s (Set s)

-- | Type used to convert into json output
data Matrices = Matrices Int [ Map Char (Matrix Int) ]
  deriving (Show, Eq)

instance ToJSON Matrices where
  toJSON (Matrices inputLength listOfMapOfMatrices)
    = object [ "steps" .= toJSON getSteps
             , "outputs" .= toJSON ([["false", "true"]] :: [[String]])
             ]
        where
          getSteps = map go (zip [0 :: Int ..] listOfMapOfMatrices)
          go (i, map') =
            let Object hm = toJSON map'
                Object o = object [ pack "position" .= toJSON i ]
            in Object (hm <> o)

instance ToJSON (Matrix Int) where
  toJSON = toJSON . toLists

-- | AST for Regular Expressions
data Reg a
  = Union (Reg a) (Reg a)
  -- ^ Alternative, ex. (a|b)
  | Cat (Reg a) (Reg a)
  -- ^ Concatenation, ex. (ab)
  | Rep (Reg a)
  -- ^ Repetition, ex. (a)*
  | Lit a
  -- ^ Literal, ex. a
  | Eps
  -- ^ Epsilon, ex. ""
  deriving (Show, Data)

instance Functor Reg where
  fmap _ Eps         = Eps
  fmap f (Lit x)     = Lit (f x)
  fmap f (Rep x)     = Rep (f <$> x)
  fmap f (Cat x y)   = Cat (f <$> x) (f <$> y)
  fmap f (Union x y) = Union (f <$> x) (f <$> y)

infixr 9 `Union`

-- | ENFA
data ENFA s a
  = ENFA { trans :: Transition s a
         -- ^ Transitions in the ENFA
         , start :: s
         -- ^ Initial starting state
         , final :: Set s
         -- ^ Final states
         , states :: Set s
         -- ^ List of all states
         } deriving (Show, Eq)

type Stack a = [a]
