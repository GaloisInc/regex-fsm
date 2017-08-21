{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regex.Closure where

import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S

import           Regex.ENFA
import           Regex.Types

-- | The ε-closure of an NFA state q is the set containing q along with all states in
-- the automaton that are reachable by any number of ε-transitions from q
-- `getClosure` constructs the map of Epsilon closures for ever state in our ENFA
getClosure :: Ord s => Ord a => ENFA s a -> EpsClosure s
getClosure ENFA {..} = M.fromList $ zip xs $ flip dfs trans <$> xs
  where
    xs = S.toList states
    dfs s trans' = epsClosure $ execState go DFSState {
      toVisit = [s]
    , epsClosure = S.singleton s
    , visited = mempty
    } where
        go = fix $ \loop -> do
          maybeS <- popStack
          forM_ maybeS $ \someState ->
            forM_ (M.lookup someState trans') $ \transitionMap ->
              forM_ (M.lookup Nothing transitionMap) $ \epsTransitions -> do
                forM_ epsTransitions $ \e -> do
                  visited <- hasVisited e
                  unless visited $ do
                    markVisited e
                    pushStack e
                    addToClosure e
                    loop
        addToClosure v = do
          s' <- get
          ec <- gets epsClosure
          put s' { epsClosure = S.insert v ec }

hasVisited
  :: (MonadState (DFSState a) f, Eq a)
  => a
  -> f Bool
hasVisited s =
  elem s <$> gets visited

markVisited
  :: MonadState (DFSState a) m
  => a
  -> m ()
markVisited e = modify $ \s ->
  s { visited = e : visited s }

popStack :: State (DFSState s) (Maybe s)
popStack = do
  ns <- get
  let (pos, newStack) = pop (toVisit ns)
  put ns { toVisit = newStack }
  return pos
    where
      pop :: [a] -> (Maybe a, [a])
      pop [] = (Nothing, [])
      pop (x:xs) = (Just x, xs)

pushStack :: s -> State (DFSState s) ()
pushStack pos = do
  modify $ \ns -> ns {
    toVisit = pos : toVisit ns
  }

data DFSState s = DFSState {
    toVisit    :: [s]
  , visited    :: [s]
  , epsClosure :: Set s
  } deriving (Show, Eq)
