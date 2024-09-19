module Data.Ultrafilter
  ( Subset (..),
    Ultrafilter (runUltrafilter),
  )
where

import Data.Functor.Contravariant

-- | Power set of @a@.
newtype Subset a = Subset {runSubset :: a -> Bool}

-- | @'contramap'@ takes each function @f@ to the preimage map of @f@.
--   @'Subset'@ is also a covariant functor, but the @'Functor'@ instance cannot be written in haskell because it requires quantifying over all values of a type.
instance Contravariant Subset where
  contramap =
    (Subset .)
      . (. runSubset)
      . flip (.)

-- | An ultrafilter on @a@ is a set @U@ of subsets of @a@ with the property that, for every finite partition of @a@
--   exactly one set in the partition is an element of @U@.
--   Since not all subsets of the power set of @a@ have this property, the constructor is not exported.
newtype Ultrafilter a = Ultrafilter {runUltrafilter :: Subset (Subset a)}

instance Functor Ultrafilter where
  fmap =
    (Ultrafilter .)
      . (. runUltrafilter)
      . contramap
      . contramap

instance Applicative Ultrafilter where
  pure = Ultrafilter . Subset . (. runSubset) . flip ($)
  (<*>) = (. flip fmap) . (>>=)

instance Monad Ultrafilter where
  (>>=) =
    ( ( Ultrafilter
          . Subset
          . (. Subset . (. (runSubset . runUltrafilter)) . flip ($))
          . runSubset
          . runUltrafilter
      )
        .
    )
      . flip fmap
