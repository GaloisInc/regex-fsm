module Regex.Secretive where

import           Data.Set (Set)
import qualified Data.Set as S
import           Math.Conjunction

type SecretIndex = Int

-- | A secretive value is either a value or a symbolic variable.
data Secretive a = Secret SecretIndex | Public a
  deriving (Eq, Ord, Read, Show)

-- | When converting from an epsilon NFA over 'Secretive' values to a DFA, we
-- will end up with transitions labeled by zeroth-order propositions whose
-- atoms are 'Secretive' values. In the particular algorithm we are using for
-- now, these will in fact just be big conjunctions of 'Public' values and
-- (possibly negated) 'Secret' values. Because of this restricted format, we
-- can create a convenient normal form. 'SecretiveTransition' is that normal
-- form.
data SecretiveTransition a = SecretiveTransition
  { exactMatch :: Maybe a
  , conditions :: Conjunction SecretIndex
  } deriving (Eq, Ord, Read, Show)

-- | If @dnf alphabet ss = ts@, then @ts@ are mutually exclusive (any pairwise
-- conjunction is equivalent to @False@), are complete (their disjunction is
-- equivalent to @True@ for alphabet @alphabet@), and cover @ss@ (each element
-- of @ss@ is equivalent to some disjunction of elements of @ts@).
--
-- The secretive transitions are also not too fine, but I don't know how to
-- state what I mean there precisely. In any case the 'exactMatch' is 'Nothing'
-- if possible, and we don't emit extra bits in the 'conditions'.
dnf :: Set a -> [Secretive a] -> [SecretiveTransition a]
dnf alphabet ss = do
    exactMatch <- if useExactMatches
                  then Just <$> S.toList alphabet
                  else [Nothing]
    condition <- allConjunctions secrets
    return (SecretiveTransition exactMatch condition)
  where
  secrets = S.fromList [i | Secret i <- ss]
  useExactMatches = not . null $ [() | Public _ <- ss]

implies :: Ord a => SecretiveTransition a -> Secretive a -> Bool
implies (SecretiveTransition exactMatch conditions) s = case s of
  Public a -> exactMatch == Just a
  Secret i -> i `S.member` positive conditions
