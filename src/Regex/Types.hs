{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Regex.Types where

import           Control.Monad.State
import           Data.Default
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Set            (Set, insert)

type Transition s a = Map s (Map (Maybe a) (Set s))
type EpsClosure s = M.Map s (Set s)

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
  deriving (Show)

instance Functor Reg where
  fmap _ Eps = Eps
  fmap f (Lit x) = Lit (f x)
  fmap f (Rep x) = Rep (f <$> x)
  fmap f (Cat x y) = Cat (f <$> x) (f <$> y)
  fmap f (Union x y) = Union (f <$> x) (f <$> y)

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

-- data DFSState s a = DFSState {
--     toVisit  :: Stack s
--   , visited  :: Set s
--   , dfsState :: a
--   } deriving (Show, Eq)

-- -- | Class to help in performing DFS
-- class ( Eq state
--       , Default a
--       , MonadState (DFSState state a) m
--       , Ord state
--       ) => DFS m state a where
--   markVisited :: state -> m ()
--   -- ^ Mark a state as visited
--   markVisited e = modify $ \s ->
--     s { visited = insert e (visited s) }

--   hasVisited :: state -> m Bool
--   -- ^ Check if we have seen this state before, cycle detection
--   hasVisited s = elem s <$> gets visited

--   popStack :: m (Maybe state)
--   -- ^ Pop element off the stack, if exists
--   popStack = do
--     ns <- get
--     let (pos, newStack) = pop (toVisit ns)
--     put ns { toVisit = newStack }
--     return pos
--       where
--         pop :: [a] -> (Maybe a, [a])
--         pop [] = (Nothing, [])
--         pop (x:xs) = (Just x, xs)

--   pushStack :: state -> m ()
--   -- ^ Push element onto the stack
--   pushStack pos = do
--    modify $ \ns -> ns {
--      toVisit = pos : toVisit ns
--    }

-- -- | DFA
-- data DFA s a
--   = DFA { trans :: Map (s, a) s
--         -- ^ Transitions in the DFA
--         , start :: s
--         -- ^ Initial starting state
--         , finals :: Set s
--         -- ^ Final states
--         } deriving (Show, Eq)
