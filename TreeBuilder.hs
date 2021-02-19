module TreeBuilder where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (left, y, right) -> Node (unfold f left) y (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold go 0
 where
  go y
    | y == x = Nothing
    | otherwise = Just (y + 1, y, y + 1)