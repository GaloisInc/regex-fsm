module Math.Conjunction
  ( Conjunction
  , allConjunctions
  , positive, negative
  ) where

import           Data.Set (Set)
import qualified Data.Set as S

-- Invariant: the `positive` and `negative` sets are disjoint.
-- | A conjunction of atoms of type @a@ and their formal negation.
data Conjunction a = Conjunction { positive, negative :: Set a }
  deriving (Eq, Ord, Read, Show)

-- | Given a set of atoms, form all conjunctions that contain one positive and
-- one negative from each.
allConjunctions :: Ord a => Set a -> [Conjunction a]
allConjunctions atoms = do
  props <- mapM posNeg (S.toList atoms)
  let (p, n) = mconcat props
  return $ Conjunction p n
  where
  posNeg atom = [(s, e), (e, s)] where
    s = S.singleton atom
    e = S.empty
