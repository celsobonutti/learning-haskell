import Data.Monoid
import Four
import Pair
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Three
import Two
import Validation

validation :: Validation (String, String, String) (String, String, String)
validation = Success' ("b", "w", "l")

prop_validation = quickBatch $ applicative validation

pair :: Pair (String, [Int], Sum Int)
pair = Pair ("b", [1], Sum 4) ("a", [2, 4], Sum 1)

prop_pair = quickBatch $ applicative pair

two :: Two (String, [Int], Sum Int) ([Int], [Int], [Int])
two = Two ("memes", [1], Sum 3) ([2], [5], [9])

prop_two = quickBatch $ applicative two

type Tuple = ([Int], [Int], [Int])

tuple = ([1], [1], [1])

three :: Three Tuple Tuple Tuple
three = Three tuple tuple tuple

prop_three = quickBatch $ applicative three

three' :: Three' Tuple Tuple
three' = Three' tuple tuple tuple

prop_three' = quickBatch $ applicative three'

four :: Four Tuple Tuple
four = Four tuple tuple tuple tuple

prop_four = quickBatch $ applicative four

main :: IO ()
main = do
  prop_validation
  prop_pair
  prop_two
  prop_three
  prop_three'
  prop_four
