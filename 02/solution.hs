parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

listPred :: [Int] -> Bool
listPred list = checkList (\x y -> x < y && y - x < 4) list || checkList (\x y -> x > y && x - y < 4) list
    where
        checkList :: (Int -> Int -> Bool) -> [Int] -> Bool
        checkList pred xs = all (uncurry pred) $ zip xs (tail xs)

partOne :: String -> Int
partOne input = length $ filter listPred $ parseInput input

partTwo :: String -> Int
partTwo input = length $ filter (\list -> any listPred [removeAt i list | i <- [0..(length list)]]) $ parseInput input
    where
        removeAt _ [] = []
        removeAt i (a:as)
           | i == 0    = as
           | otherwise = a : removeAt (i-1) as
