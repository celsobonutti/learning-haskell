import Identity
import List
import Nope
import Sum
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type Tuple = (String, String, String)

monad_prop x = quickBatch $ monad x

tuple :: Tuple
tuple = ("Oi", "seu", "lindo(a)")

sum' :: Sum Tuple Tuple
sum' = Second tuple

nope :: Nope Tuple
nope = NopeDotJpg

identity :: Identity Tuple
identity = Identity tuple

list :: List Tuple
list = Cons tuple Nil

main :: IO ()
main = do
  monad_prop sum'
  monad_prop nope
  monad_prop identity
  monad_prop list
