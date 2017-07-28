{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regex.NFA where

import           Control.Monad.State
import           Data.Function
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Regex.ENFA

-- | NFA
data NFA s a
  = NFA { trans :: Trans' s a
        -- ^ Transitions in the NFA
        , start :: s
        -- ^ Initial starting state
        , final :: Set s
        -- ^ Final states
        } deriving (Show, Eq)

type EpsClosure s = M.Map s (S.Set s)

emptyState :: Ord s => DFSState s
emptyState = DFSState mempty mempty mempty

-- | The ε-closure of an NFA state q is the set containing q along with all states in
-- the automaton that are reachable by any number of ε-transitions from q
test' :: IO ()
test' = do
    print start
    mapM_ print (M.assocs trans)
    mapM_ print final
    putStrLn "========"
    mapM_ print states
  where
    ENFA {..} = enfa

type DFAState s a = M.Map s (M.Map a s, Set s)

enfa :: ENFA Integer Char
enfa = toENFA regex

regex :: Reg Char -- (ba*b)
regex = Lit 'b' `Cat` Rep (Lit 'a') `Cat` Lit 'b'

-- | I need closure
getClosure :: (Ord s, Ord a) => ENFA s a -> M.Map s (Set s)
getClosure ENFA {..} =
  M.fromList $ zip xs $ flip dfs trans <$> xs
    where
      xs = S.toList states

-- type Trans s a = M.Map s (M.Map (Maybe a) (S.Set s))

-- | -- type Trans s a = M.Map s (M.Map (Maybe a) (S.Set s))
dfs :: (Ord a, Ord s) => s -> Trans s a -> Set s
dfs start trans = epsClosure $ execState go emptyState {
    toVisit = [start]
  , epsClosure = S.singleton start
  } where
    go = fix $ \loop -> do
      maybeS <- popStack
      forM_ maybeS $ \someState ->
        forM_ (M.lookup someState trans) $ \transitionMap ->
          forM_ (M.lookup Nothing transitionMap) $ \epsTransitions -> do
            forM_ epsTransitions $ \e -> do
              visited <- hasVisited e
              unless visited $ do
                markVisited e
                pushStack e
                addToClosure e
                loop

data DFSState s = DFSState {
    toVisit    :: [s]
  , visited    :: [s]
  , epsClosure :: Set s
  } deriving (Show, Eq)

addToClosure
  :: (Ord s, MonadState (DFSState s) m)
  => s
  -> m ()
addToClosure v = do
  s <- get
  ec <- gets epsClosure
  put s { epsClosure = S.insert v ec }

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

