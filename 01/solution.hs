import Data.List (transpose, sort)

parseInput input = map sort $ transpose $ map (map read . words) (lines input)

partOne :: String -> Int
partOne input = sum $ uncurry (zipWith (\x y -> abs $ x - y)) $ unfuck $ parseInput input
    where
        unfuck [a, b] = (a, b)

partTwo :: String -> Int
partTwo input = go $ parseInput input
    where
        go [a, b] = sum (map (\x -> x * length (filter (==x) b)) a)

