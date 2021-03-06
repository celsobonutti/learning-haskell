{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.Monoid (Sum, Product)

import qualified Trivial
import qualified Identity
import qualified Two
import qualified Three
import qualified Four
import qualified BoolConj
import qualified BoolDisj
import qualified Or
import qualified Validation

semigroupAssoc ::
  (Eq m, Semigroup m) =>
  m ->
  m ->
  m ->
  Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

prop_trivialAssoc :: Trivial.Assoc
prop_trivialAssoc = semigroupAssoc

prop_identityStringAssoc :: Identity.Assoc String
prop_identityStringAssoc = semigroupAssoc

prop_twoSumStringAssoc :: Two.Assoc (Sum Int) String
prop_twoSumStringAssoc = semigroupAssoc

prop_threeSumProductIntListAssoc :: Three.Assoc (Sum Int) (Product Int) [Int]
prop_threeSumProductIntListAssoc = semigroupAssoc

prop_fourSumProductIntListFloatListAssoc :: Three.Assoc (Sum Int) (Product Int) [Int]
prop_fourSumProductIntListFloatListAssoc = semigroupAssoc

prop_boolConj :: BoolConj.Assoc
prop_boolConj = semigroupAssoc

prop_boolDisj :: BoolDisj.Assoc
prop_boolDisj = semigroupAssoc

prop_orIntString :: Or.Assoc Int String
prop_orIntString = semigroupAssoc

prop_or :: Or.Or Int Char -> Or.Or Int Char -> Bool
prop_or = Or.prop_or

prop_validation :: Validation.Assoc String Int
prop_validation = semigroupAssoc

return []

check :: IO Bool 
check = $quickCheckAll

main :: IO Bool
main = check