{-# LANGUAGE RecordWildCards #-}
module Regex.NFA where

import qualified Data.Map    as M
import           Data.Monoid
import           Data.Set    (Set)
import qualified Data.Set    as S

import           Regex.ENFA

-- | NFA
data NFA s a
  = NFA { trans :: Set (s,a,s)
        -- ^ Transitions in the NFA
        , start :: s
        -- ^ Initial starting state
        , final :: Set s
        -- ^ Final states
        } deriving (Show, Eq)

type EpsClosure s = M.Map s (S.Set s)

-- toNFA :: ENFA s a -> NFA s a
-- toNFA enfa@ENFA {..} = go (createClosure enfa)
--   where
--     go = undefined
--     createClosure ENFA {..} =
--       M.fromList [ (s, fromList y)
--                  | move <- S.toList
--                  ]

-- | Retrieves all the states out of an ENFA into a `Set a`.
-- TODO: Test that calling `getStates` on the correct construction of an `ENFA`
-- always results in a Set with all states present in the `ENFA`
getStates :: Ord s => ENFA s a -> Set s
getStates ENFA {..} =
  S.foldr (S.union) S.empty $
    flip S.map trans $ \move ->
      S.fromList $ case move of
        Move s _ f -> [s,f]
        EMove s f  -> [s,f]

-- | Test epsilon closure can be constructed successfully
constructEpsilonClosure :: Ord s => ENFA s a -> EpsClosure s
constructEpsilonClosure enfa@ENFA {..} = go M.empty $ S.toList (getStates enfa)
  where
    go closure [] = closure
    go closure (x:xs) =
      go (M.insert x states closure) xs
        where
          -- Current state must be added,
          -- along with all states reachable by a single epsilon move
          states   = S.singleton x <> epsMoves
          epsMoves = S.map (\(EMove _ f) -> f) $
            flip S.filter trans $ \move ->
              case move of
                Move _ _ _ -> False
                EMove s f -> s == x

hey :: IO ()
hey = mapM_ print $ M.toList $
  constructEpsilonClosure enfa

-- | Test epsilon closure
testEps :: Show s => EpsClosure s -> IO ()
testEps = mapM_ print

-- | Test epsilon
k :: EpsClosure Int
k = M.fromList [ (1, S.fromList [1,2,4])
               , (2, S.fromList [2])
               , (3, S.fromList [3])
               , (4, S.fromList [4])
               , (5, S.fromList [4,5])
               ]

test :: Bool
test = k == constructEpsilonClosure enfa

enfa :: ENFA Int Char
enfa = ENFA {
         trans = S.fromList [ EMove 1 2
                            , EMove 1 4
                            , EMove 5 4
                            , Move 2 'a' 3
                            , Move 4 'b' 5
                            ]
         -- ^ Transitions in the ENFA
         , start = 1
         -- ^ Initial starting state
         , final = S.fromList [ 3, 4 ]
         -- ^ Final states
         }

type FinalStates s = Set s

-- | If a state has any final states in its epsilon closure
-- it too should be marked as final
isFinal :: Ord a => a -> FinalStates a -> EpsClosure a -> Bool
isFinal x finals eps = x `S.member` finals || inClosure
  where
    inClosure =
      case M.lookup x eps of
        Nothing -> False
        Just v ->
          or $ S.toList
             $ S.map (`S.member` finals) v

constructNFA :: (Ord s, Ord a) => FinalStates s -> EpsClosure s -> NFA s a
constructNFA finals closure =
  NFA { trans = S.fromList []
      , start = undefined
      , final = S.fromList []
      }

getEpsilonAdj :: ENFA s a -> [s]
getEpsilonAdj ENFA {..} = undefined
