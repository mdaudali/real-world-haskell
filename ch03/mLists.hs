import Data.List
import Data.Ord

lSort :: [[a]] -> [[a]]
lSort xs = sortBy (comparing length) xs

