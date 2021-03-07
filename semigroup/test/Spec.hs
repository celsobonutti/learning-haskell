{-# LANGUAGE TemplateHaskell #-}

import BoolConj (BoolConj)
import qualified BoolConj
import BoolDisj (BoolDisj)
import qualified BoolDisj
import Combine (Combine (..))
import qualified Combine
import Compose (Compose (..))
import Data.Monoid (Product, Sum)
import qualified Four
import Identity (Identity)
import qualified Identity
import Mem (Mem (..))
import qualified Mem
import Or (Or)
import qualified Or
import Test.QuickCheck
import qualified Three
import qualified Trivial
import Two (Two)
import qualified Two
import qualified Validation

semigroupAssoc ::
  (Eq m, Semigroup m) =>
  m ->
  m ->
  m ->
  Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
  a <> mempty == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  mempty <> a == a

prop_trivialAssoc :: Trivial.Assoc
prop_trivialAssoc = semigroupAssoc

prop_trivialLeftId :: Trivial.Identity
prop_trivialLeftId = monoidLeftIdentity

prop_trivialRightId :: Trivial.Identity
prop_trivialRightId = monoidRightIdentity

prop_identityStringAssoc :: Identity.Assoc String
prop_identityStringAssoc = semigroupAssoc

prop_identityLeftId :: Identity.Identity String -> Bool
prop_identityLeftId = monoidLeftIdentity

prop_identityRightId :: Identity.Identity String -> Bool
prop_identityRightId = monoidRightIdentity

prop_twoSumStringAssoc :: Two.Assoc (Sum Int) String
prop_twoSumStringAssoc = semigroupAssoc

prop_twoLeftId :: Two (Product Int) (Sum Int) -> Bool
prop_twoLeftId = monoidLeftIdentity

prop_twoRightId :: Two String [Int] -> Bool
prop_twoRightId = monoidRightIdentity

prop_threeSumProductIntListAssoc :: Three.Assoc (Sum Int) (Product Int) [Int]
prop_threeSumProductIntListAssoc = semigroupAssoc

prop_fourSumProductIntListFloatListAssoc :: Three.Assoc (Sum Int) (Product Int) [Int]
prop_fourSumProductIntListFloatListAssoc = semigroupAssoc

prop_boolConj :: BoolConj.Assoc
prop_boolConj = semigroupAssoc

prop_boolConjLeftId :: BoolConj -> Bool
prop_boolConjLeftId = monoidLeftIdentity

prop_boolConjRightId :: BoolConj -> Bool
prop_boolConjRightId = monoidRightIdentity

prop_boolDisj :: BoolDisj.Assoc
prop_boolDisj = semigroupAssoc

prop_boolDisjLeftId :: BoolDisj -> Bool
prop_boolDisjLeftId = monoidLeftIdentity

prop_boolDisjRightId :: BoolDisj -> Bool
prop_boolDisjRightId = monoidRightIdentity

prop_orIntString :: Or.Assoc Int String
prop_orIntString = semigroupAssoc

prop_orAssoc :: Or.Or Int Char -> Or.Or Int Char -> Bool
prop_orAssoc = Or.prop_or

prop_combineAssoc :: String -> Combine String [Int] -> Combine String [Int] -> Combine String [Int] -> Bool
prop_combineAssoc x (Combine f) (Combine g) (Combine h) =
  (f <> (g <> h)) x == ((f <> g) <> h) x

prop_combineLeftId :: [Int] -> Combine [Int] String -> Bool
prop_combineLeftId x (Combine f) =
  (f <> mempty) x == f x

prop_combineRightId :: [Int] -> Combine [Int] String -> Bool
prop_combineRightId x (Combine f) =
  (mempty <> f) x == f x

prop_composeLeftId :: [Int] -> Compose [Int] -> Bool
prop_composeLeftId x (Compose f) =
  (f <> mempty) x == f x

prop_composeRightId :: [Int] -> Compose [Int] -> Bool
prop_composeRightId x (Compose f) =
  (mempty <> f) x == f x

prop_composeAssoc :: [Int] -> Compose [Int] -> Compose [Int] -> Compose [Int] -> Bool
prop_composeAssoc x (Compose f) (Compose g) (Compose h) =
  (f <> (g <> h)) x == ((f <> g) <> h) x

prop_validation :: Validation.Assoc String Int
prop_validation = semigroupAssoc

prop_memAssoc :: String -> Mem String [Int] -> Mem String [Int] -> Mem String [Int] -> Bool
prop_memAssoc x (Mem f) (Mem g) (Mem h) =
  (f <> (g <> h)) x == ((f <> g) <> h) x

prop_memLeftId :: String -> Mem String [Int] -> Bool
prop_memLeftId x (Mem f) =
  (f <> mempty) x == f x

prop_memRightId :: String -> Mem String [Int] -> Bool
prop_memRightId x (Mem f) =
  (mempty <> f) x == f x

return []

check :: IO Bool
check = $quickCheckAll

main :: IO Bool
main = check
