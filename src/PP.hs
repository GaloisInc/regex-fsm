{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module PP (PP(pp)) where

import           Math.Conjunction
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)
import           Regex

class PP a where pp :: a -> String

instance PP Int  where pp = show
instance PP Char where pp = show

instance (Ord a, PP a) => PP (Conjunction a) where
  pp c = id
    . intercalate " ∧ "
    . map ppAtom
    . S.toAscList
    $ S.union (S.map (flip(,)True ) (positive c))
              (S.map (flip(,)False) (negative c))
    where
    ppAtom (x, b) = (if b then ' ' else '¬') : pp x

instance PP a => PP (SecretiveTransition a) where
  pp trans = foldMap pp (exactMatch trans) ++ "\t"
           ++ pp (conditions trans)

instance (Ord a, PP a) => PP (Set a) where
  pp = bracket . intercalate "," . map pp . S.toAscList where
    bracket s = "{" ++ s ++ "}"

newtype Edge s a = Edge { getEdge :: (s, a, s) }

instance (PP s, PP a) => PP [Edge s a] where
  pp transitions = unlines ppTriples where
    (ls, ms, rs) = unzip3 . map getEdge $ transitions

    ppL = map pp ls
    ppM = map pp ms
    ppR = map pp rs

    lenL = maximum . (0:) . map length $ ppL
    lenR = maximum . (0:) . map length $ ppR

    padR n s = s ++ replicate (n - length s) ' '

    paddedL = map (padR lenL) ppL
    paddedR = map (padR lenR) ppR

    ppTriples = zipWith3 (\l m r -> l ++ " -> " ++ r ++ "    " ++ m) paddedL ppM paddedR

instance (Ord s, PP s, PP a) => PP (DFA s a) where
  pp DFA{..} = "-> " ++ pp start ++ " -> " ++ pp finals ++ "\n" ++ pp
    [Edge (src, edge, tgt) | (src, ets) <- M.toAscList trans, (edge, tgt) <- M.toAscList ets]
