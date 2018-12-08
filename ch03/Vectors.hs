data Direction = LeftTurn | RightTurn | Straight deriving (Show)
data Point = Point Double Double

getDirection (Point x1 y1) (Point x2 y2) (Point x3 y3) 
    | ((x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)) > 0 = LeftTurn
    | ((x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)) < 0 = RightTurn
    | otherwise = Straight

getTurns ([]) = []
getTurns ([_]) = []
getTurns ([_, _]) = []
getTurns (a:b:c:xs) = (getDirection a b c) : (getTurns (b:c:xs))

