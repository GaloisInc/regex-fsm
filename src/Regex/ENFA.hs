{-# LANGUAGE OverloadedStrings #-}
module Regex.ENFA
  ( -- * Types
    Reg  (..)
  , Move (..)
  , ENFA (..)
  , Graph
  ) where

import           Control.Monad.State
import           Data.Graph.Inductive (Gr, mkGraph)
import qualified Data.Graph.Inductive as G
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

-- | Graph representation of ENFA
type Graph a = Gr Int (Move Int a)

-- | For representing transitions in the ENFA
-- includes epsilon transitions
data Move s a
  = Move s a s
  -- ^ A move in the ENFA
  | EMove s s
  -- ^ An epsilon move in the ENFA
  deriving (Show, Eq, Ord)

-- | ENFA
data ENFA s a
  = ENFA { trans :: Set (Move s a)
         -- ^ Transitions in the ENFA
         , start :: s
         -- ^ Initial starting state
         , final :: Set s
         -- ^ Final states
         } deriving (Show, Eq)

newState :: Num s => State s s
newState = modify (+1) >> get

test' = print (toENFA e)
  where
    e = Lit 'a' `Union` (Rep (Lit 'b'))

toENFA :: (Ord s, Num s, Ord a) => Reg a -> ENFA s a
toENFA = flip evalState 0 . go
  where
    -- | "a|b"
    go (a' `Union` b') = do
      (a, b) <- liftM2 (,) (go a') (go b')
      n <- newState
      pure ENFA {
        start = n
      , final = mconcat [ final a, final b ]
      , trans = mconcat [
            trans a
          , trans b
          , S.fromList [
              EMove n (start a)
            , EMove n (start b)
            ]
          ]
      }
    -- | "ab"
    go (a' `Cat` b') = do
      a <- go a'
      b <- go b'
      pure ENFA {
        start = start a
      , final = final b
      , trans = mconcat [
            trans a
          , trans b
          , S.fromList [
                EMove x (start b)
              | x <- S.toList (final a)
              ]
          ]
      }
    -- | "a*"
    go (Rep a') = do
      a <- go a'
      pure a {
        trans = trans a <> f (start a) a
      , final = S.singleton (start a)
      }
        where
          f s a = S.map (flip EMove s) (final a)

    -- | "a"
    go (Lit x) = do
      start' <- newState
      final' <- newState
      pure ENFA {
        start = start'
      , trans = S.fromList [ Move start' x final' ]
      , final = S.singleton final'
      }
    -- | ""
    go Eps = do
      n <- newState
      pure $ ENFA mempty n (S.singleton n)

toGraph :: ENFA s a -> Graph a
toGraph = undefined

-- | Show EENFA as Graph
enfa :: Graph Char
enfa = mkGraph nodes edges
  where
    nodes =  [ one, two, three ]
    edges =  [ (1, 2, Move 1 'a' 2)
             , (2, 3, Move 2 'b' 3)
             ]
    [one,two,three] = [(1,1),(2,2),(3,3)]

-- | Execute a string against this EENFA
run :: String -> ENFA s a -> Bool
run = undefined
