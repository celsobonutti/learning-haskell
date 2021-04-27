import Big
import Constant
import Identity
import List
import Option
import SkiFree
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Tree

type Tuple = (Int, Double, String)

traversableProp x = quickBatch $ traversable x

main :: IO ()
main = do
  traversableProp (undefined :: Identity Tuple)
  traversableProp (undefined :: Option Tuple)
  traversableProp (undefined :: S Maybe Tuple)
  traversableProp (undefined :: List Tuple)
  traversableProp (undefined :: Big String Tuple)
  traversableProp (undefined :: Tree Tuple)
  traversableProp (undefined :: Constant String Tuple)
