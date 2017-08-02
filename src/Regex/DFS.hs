{-# LANGUAGE FlexibleContexts #-}
module Regex.DFS where

import           Data.Default
import           Control.Monad.State
import           Data.Set            (empty)

import           Regex.Types

-- | Runs DFS
runDFS :: Default a => State (DFSState s a) () -> a
runDFS go = dfsState $ flip execState (DFSState mempty empty def) go

    -- Empty DFA state
emptyState = DFSState mempty mempty mempty

hasVisited s = elem s <$> gets visited

markVisited e =
  modify $ \s -> s {
    visited = e : visited s
  }

popStack = do
  ns <- get
  let (pos, newStack) = pop (toVisit ns)
  put ns { toVisit = newStack }
  return pos
    where
      pop :: [a] -> (Maybe a, [a])
      pop [] = (Nothing, [])
      pop (x:xs) = (Just x, xs)

pushStack :: s -> State (DFSState s a) ()
pushStack p = modify $ \ns -> ns { toVisit = p : toVisit ns }
