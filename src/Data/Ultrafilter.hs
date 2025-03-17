{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Ultrafilter
  ( Subset (..),
    singleton,
    Ultrafilter,
    getUltrafilter,
    runUltrafilter,
    CompactHausdorff (..),
  )
where

import Control.Monad
import Data.Either (isLeft)
import Data.Functor.Contravariant
import Data.Maybe (fromMaybe, isNothing)
import GHC.Generics

-- | Power set of @a@.
newtype Subset a = Subset {elementOf :: a -> Bool}

singleton :: (Eq a) => a -> Subset a
singleton = Subset . (==)

-- | @'contramap'@ takes each function @f@ to the preimage map of @f@.
--   @'Subset'@ is also a covariant functor, but the @'Functor'@ instance cannot be written in haskell because it requires quantifying over all values of a type.
instance Contravariant Subset where
  contramap = (Subset .) . (. elementOf) . flip (.)

-- | An ultrafilter on @a@ is a set @U@ of subsets of @a@ with the property that, for every finite partition of @a@
--   exactly one set in the partition is an element of @U@.
--   Since not all subsets of the power set of @a@ have this property, the constructor is not exported.
data Ultrafilter a = Ultrafilter !(Subset (Subset a))

getUltrafilter :: Ultrafilter a -> Subset (Subset a)
getUltrafilter (Ultrafilter u) = u

runUltrafilter :: Ultrafilter a -> Subset a -> Bool
runUltrafilter = elementOf . getUltrafilter

instance Functor Ultrafilter where
  fmap f (Ultrafilter u) = Ultrafilter $ contramap (contramap f) u

instance Applicative Ultrafilter where
  pure = Ultrafilter . Subset . flip elementOf
  (<*>) = (. flip fmap) . (>>=)

instance Monad Ultrafilter where
  (>>=) :: forall a b. Ultrafilter a -> (a -> Ultrafilter b) -> Ultrafilter b
  (>>=) = (limit .) . flip fmap

-- | A compact hausdorff space is an algebra over the ultrafilter monad.
--   Instances must satisfy the following laws:
--
--   * @'limit' '.' 'pure' = 'id'@
--   * @'limit' '.' 'join' = 'limit' '.' 'fmap' 'limit'@
class CompactHausdorff a where
  limit :: Ultrafilter a -> a
  default limit :: (Generic a, GCompact (Rep a a)) => Ultrafilter a -> a
  limit = to . gLimit @(Rep a a) . fmap from

class GCompact a where
  gLimit :: Ultrafilter a -> a

instance GCompact (V1 p) where
  gLimit _ = error "void"

instance GCompact (U1 p) where
  gLimit u = if runUltrafilter u $ singleton U1 then U1 else error "not an ultrafilter"

instance (GCompact c) => GCompact (K1 i c p) where
  gLimit = K1 . gLimit . fmap unK1

instance (GCompact (x p)) => GCompact (M1 a b x p) where
  gLimit = M1 . gLimit . fmap unM1

instance (GCompact (x p), GCompact (y p)) => GCompact ((x :+: y) p) where
  gLimit u =
    if runUltrafilter u . Subset $ \case L1 _ -> True; R1 _ -> False
      then
        L1 . gLimit . Ultrafilter
          $ contramap
            (\s -> Subset $ \case L1 x -> elementOf s x; R1 _ -> False)
          $ getUltrafilter u
      else
        R1 . gLimit . Ultrafilter
          $ contramap
            (\s -> Subset $ \case L1 _ -> False; R1 x -> elementOf s x)
          $ getUltrafilter u

instance (GCompact (x p), GCompact (y p)) => GCompact ((x :*: y) p) where
  gLimit u = gLimit (flip fmap u $ \(x :*: _) -> x) :*: gLimit (flip fmap u $ \(_ :*: x) -> x)

instance CompactHausdorff (Ultrafilter a) where
  limit u = Ultrafilter . Subset $ runUltrafilter u . Subset . flip runUltrafilter

deriving instance CompactHausdorff Bool

deriving instance CompactHausdorff Ordering

instance CompactHausdorff Char where
  limit u = head $ filter (runUltrafilter u . Subset . (==)) [minBound .. maxBound]

instance CompactHausdorff Int where
  limit u = head $ filter (runUltrafilter u . Subset . (==)) [minBound .. maxBound]

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_ : t) = Just t
safeTail _ = Nothing

instance (CompactHausdorff a) => CompactHausdorff (Maybe a) where
  limit u =
    if runUltrafilter u . Subset $ isNothing
      then Nothing
      else
        Just
          . limit
          . Ultrafilter
          . Subset
          $ runUltrafilter u . Subset . maybe True . elementOf

instance (CompactHausdorff a) => CompactHausdorff [a] where
  limit u = case limit $ fmap safeHead u of
    Nothing -> []
    Just x -> x : fromMaybe [] (limit $ fmap safeTail u)

instance (CompactHausdorff a, CompactHausdorff b) => CompactHausdorff (a, b) where
  limit u = (limit $ fmap fst u, limit $ fmap snd u)

instance (CompactHausdorff a, CompactHausdorff b) => CompactHausdorff (Either a b) where
  limit u =
    if runUltrafilter u . Subset $ isLeft
      then
        Left
          . limit
          . Ultrafilter
          . Subset
          $ runUltrafilter u . Subset . (\f -> \case Left a -> f a; Right _ -> True) . elementOf
      else
        Right
          . limit
          . Ultrafilter
          . Subset
          $ runUltrafilter u . Subset . either (const True) . elementOf

instance (CompactHausdorff b) => CompactHausdorff (a -> b) where
  limit u x = limit . fmap ($ x) $ u

instance CompactHausdorff (Subset a) where
  limit u = Subset $ runUltrafilter u . Subset . flip elementOf

instance (Eq a) => Eq (Ultrafilter a) where
  (==) = (limit .) . liftM2 (==)

instance (Ord a) => Ord (Ultrafilter a) where
  compare = (limit .) . liftM2 compare

instance (Show a) => Show (Ultrafilter a) where
  show = limit . fmap show

instance (Semigroup a) => Semigroup (Ultrafilter a) where
  (<>) = liftM2 (<>)

instance (Monoid a) => Monoid (Ultrafilter a) where
  mempty = pure mempty

instance (Num a) => Num (Ultrafilter a) where
  fromInteger = pure . fromInteger
  (+) = liftM2 (+)
  (*) = liftM2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
