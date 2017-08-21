{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Regex.Types where

import           Control.Monad.State
import           Data.Aeson
import           Data.Data
import qualified Data.HashMap.Strict as HM
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Matrix
import           Data.Monoid
import           GHC.Generics
import           Control.DeepSeq
import           Data.Set            (Set)
import           Data.Text           (Text, pack)

type Transition s a = Map s (Map (Maybe a) (Set s))
type EpsClosure s = M.Map s (Set s)

-- | A matrix branching program, together with information about how to
-- interpret the output of the program.
data MBPInterpreter = MBPInterpreter [Step] (Matrix String) deriving (Eq, Show)

-- TODO: This FromJSON instance (and supporting ones) were stolen from cryfsm.
-- We should write a library that cryfsm and this package both depend on that
-- shares a few relevant types, instances, and utilities so that we don't have
-- this code duplication.
instance FromJSON MBPInterpreter where
  parseJSON = withObject "matrix branching program" $ \o -> do
    steps   <- o .: "steps"
    when (null steps) (fail "program must have at least one step")
    outputs <- o .: "outputs"
    return (MBPInterpreter steps outputs)

type Position   = Text
type Dimensions = (Int, Int)

-- constraints:
-- * at least one matrix
-- * all matrices have the same dimensions
-- * does not branch on "position" -- this is semantically stupid, and a
--   wart left over from bad JSON format design
data Step = Step
  { sDims    :: Dimensions
  , position :: Position
  , branches :: Map Char (Matrix Int)
  } deriving (Eq, Show)

instance FromJSON Step where
  parseJSON = withObject "step" $ \o -> do
    position' <- o .: "position"
    branches' <- parseJSON (Object (HM.delete "position" o))
    case M.elems branches' of
      [] -> fail "each step must have at least one branch"
      b:bs | all (\b' -> dims b == dims b') bs -> return (Step (dims b) position' branches')
           | otherwise -> fail "all branches in the step must have the same dimensions"
    where dims matrix' = (nrows matrix', ncols matrix')

-- | Type used to convert into json output
data Matrices = Matrices Int [ Map Char (Matrix Int) ]
  deriving (Show, Eq)

instance ToJSON Matrices where
  toJSON (Matrices _ listOfMapOfMatrices)
    = object [ "steps" .= toJSON getSteps
             , "outputs" .= toJSON ([["false", "true"]] :: [[String]])
             ]
        where
          getSteps = map go (zip [0 :: Int ..] listOfMapOfMatrices)
          go (i, map') =
            let Object hm = toJSON map'
                Object o = object [ pack "position" .= toJSON (show i) ]
            in Object (hm <> o)

instance ToJSON (Matrix Int) where
  toJSON = toJSON . toLists

instance FromJSON field => FromJSON (Matrix field) where
  parseJSON v = do
    elems <- parseJSON v
    let rows = length elems
        cols:colss = map length elems
    if rows > 0 && all (cols==) colss
       then return (fromLists elems)
       else fail "expected a non-empty rectangular array of arrays"

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
  deriving (Show, Data, Generic)

instance NFData (Reg Char)

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
         } deriving (Show, Eq, Generic)

instance NFData (ENFA Int Char)

type Stack a = [a]
