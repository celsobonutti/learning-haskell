{-# LANGUAGE TemplateHaskell #-}

import BoolConj (BoolConj)
import qualified BoolConj
import BoolDisj (BoolDisj)
import qualified BoolDisj
import Data.Monoid (Product, Sum)
import qualified Four
import Identity (Identity)
import qualified Identity
import Mem (Mem)
import qualified Mem
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

prop_or :: Or.Or Int Char -> Or.Or Int Char -> Bool
prop_or = Or.prop_or

prop_validation :: Validation.Assoc String Int
prop_validation = semigroupAssoc

prop_memAssoc :: Mem.Assoc Int String
prop_memAssoc = semigroupAssoc

return []

check :: IO Bool
check = $quickCheckAll

main :: IO Bool
main = check
