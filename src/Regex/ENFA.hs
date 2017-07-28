{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Regex.ENFA
  ( -- * Types
    Reg  (..)
  , Move (..)
  , ENFA (..)
  , Graph
  , Trans
  , Trans'
  , (-->)
  , (#)
  , (##)
  , (|->)
  , (|-->)
  , (-|)
  , toENFA
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Graph.Inductive (Gr, mkGraph)
import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S

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

-- | Applicative
instance Applicative Reg where
  pure = Lit; (<*>) = undefined

-- | Alternative?
instance Alternative Reg where
  empty = Eps; (<|>) = undefined

-- | Graph representation of ENFA
type Graph a = Gr Int (Move Int a)

-- | For representing transitions in the ENFA
-- includes epsilon transitions
data Move s a
  = Move a s
  -- ^ A move in the ENFA
  | EMove s
  -- ^ An epsilon move in the ENFA
  deriving (Show, Eq, Ord)

type Trans' s a = M.Map s (M.Map a (S.Set s))
type Trans s a = M.Map s (M.Map (Maybe a) (S.Set s))

(-|) = (,)
(|->) :: (s,a) -> s -> Trans s a
(x,y) |-> s = M.singleton x $ M.singleton (Just y) (S.singleton s)

(|-->) :: (s,a) -> s -> Trans' s a
(x,y) |--> s = M.singleton x (M.singleton y (S.singleton s))

-- | ENFA
data ENFA s a
  = ENFA { trans :: Trans s a
         -- ^ Transitions in the ENFA
         , start :: s
         -- ^ Initial starting state
         , final :: Set s
         -- ^ Final states
         , states :: Set s
         -- ^ List of all states
         } deriving (Show, Eq)

newState :: Num s => State s s
newState = modify (+1) >> get

(#) :: (Ord s, Ord a) => Trans s a -> Trans s a -> Trans s a
(#) = M.unionWith (M.unionWith (S.union))
infixr 5 #

(##) :: (Ord s, Ord a) => Trans' s a -> Trans' s a -> Trans' s a
(##) = M.unionWith (M.unionWith (S.union))
infixr 5 ##

(-->) :: k -> a1 -> M.Map k (M.Map (Maybe a) (Set a1))
s --> s' = M.singleton s (M.singleton Nothing (S.singleton s'))

toENFA :: (Ord s, Num s, Ord a) => Reg a -> ENFA s a
toENFA = flip evalState 0 . go
  where
    -- | "a|b"
    go (a' `Union` b') = do
      (a, b) <- liftM2 (,) (go a') (go b')
      n <- newState
      pure ENFA {
        start = n
      , states = states a `S.union` states b `S.union` S.singleton n
      , final = mconcat [ final a, final b ]
      , trans =
          trans a # trans b
                  # n --> start a
                  # n --> start b
      }
    -- | "ab"
    go (a' `Cat` b') = do
      a <- go a'
      b <- go b'
      pure ENFA {
        start = start a
      , states = states a `S.union` states b
      , final = final b
      , trans = foldr (go' b) (trans a # trans b) (final a)
      } where
          go' b state trans =
            state --> start b # trans
    -- | "a*"
    go (Rep a') = do
      a <- go a'
      pure a {
        trans = foldr (go' a) (trans a) (final a)
      , final = S.singleton (start a)
      }
        where
          go' b state trans =
            state --> start b # trans
    -- | "a"
    go (Lit x) = do
      start' <- newState
      final' <- newState
      pure ENFA {
        start = start'
      , states = S.fromList [start',final']
      , trans = start' -| x |-> final'
      , final = S.singleton final'
      }
    -- | ""
    go Eps = do
      n <- newState
      pure $ ENFA mempty n (S.singleton n) (S.singleton n)

toGraph :: ENFA s a -> Graph a
toGraph = undefined

-- | Show EENFA as Graph
enfa :: Graph Char
enfa = mkGraph nodes edges
  where
    nodes =  [ one, two, three ]
    edges =  [ (1, 2, Move 'a' 2)
             , (2, 3, Move 'b' 3)
             ]
    [one,two,three] = [(1,1),(2,2),(3,3)]

-- | Execute a string against this EENFA
run :: String -> ENFA s a -> Bool
run = undefined
