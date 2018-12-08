import Data.Ord
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height (Empty) = 1
height (Node _ b c) = (max (height b) (height c)) + 1