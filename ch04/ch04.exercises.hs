import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast ([x]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

firstWords :: String -> String
firstWords input = unlines (map (unwords . (take 1) . words) (lines input))

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs 
    | any null xs = []
    | otherwise   = (map head xs) : (transpose (map tail xs)) 