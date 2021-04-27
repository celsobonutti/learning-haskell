module Big where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Big a b
  = Big a b b
  deriving (Eq, Show, Ord)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary =
    liftA3 Big arbitrary arbitrary arbitrary
