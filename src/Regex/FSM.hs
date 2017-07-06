{-# LANGUAGE OverloadedStrings #-}
module Regex.FSM where

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

-- | Graph representation of NFA
type Graph a = Gr Int (Move Int a)

-- | For representing transitions in the NFA
-- includes epsilon transitions
data Move s a
  = Move s a s
  -- ^ A move in the NFA
  | EMove s s
  -- ^ An epsilon move in the NFA
  deriving (Show, Eq, Ord)

-- | NFA
data NFA s a
  = NFA { trans :: Set (Move s a)
        -- ^ Transitions in the NFA
        , start :: s
        -- ^ Initial starting state
        , final :: Set s
        -- ^ Final states
        } deriving (Show, Eq)

newState :: Num s => State s s
newState = modify (+1) >> get

toENFA :: (Ord s, Num s, Ord a) => Reg a -> NFA s a
toENFA = flip evalState 0 . go
  where
    -- | "a|b"
    go (a' `Union` b') = do
      (a, b) <- liftM2 (,) (go a') (go b')
      n <- newState
      pure NFA {
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
      pure NFA {
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
      pure NFA {
        start = start'
      , trans = S.fromList [ Move start' x final' ]
      , final = S.singleton final'
      }
    -- | ""
    go Eps = pure $ NFA mempty 0 mempty

toGraph :: NFA s a -> Graph a
toGraph = undefined
  where
    -- | "a|b"
    go (l `Union` r) v = undefined
    -- | "ab"
    go (l `Cat` r) v = undefined
    -- | "a*"
    go (Rep r) v = undefined
    -- | "a"
    go (Lit x) v = undefined
    -- | ""
    go Eps 0 = undefined

-- | Show ENFA as Graph
enfa :: Graph Char
enfa = mkGraph nodes edges
  where
    nodes =  [ one, two, three ]
    edges =  [ (1, 2, Move 1 'a' 2)
             , (2, 3, Move 2 'b' 3)
             ]
    [one,two,three] = [(1,1),(2,2),(3,3)]

-- | Execute a string against this ENFA
run :: String -> NFA s a -> Bool
run = undefined
