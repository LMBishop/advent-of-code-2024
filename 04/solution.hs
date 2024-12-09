import qualified Data.Map as M
import Data.Maybe (catMaybes)

type Coord = (Int, Int)

type Grid = M.Map Coord Char

type Direction = Coord -> Coord

readGrid :: [[Char]] -> Grid
readGrid input = M.fromList [ ((i, j), char) | (j, row) <- zip [0 ..] input, (i, char) <- zip [0 ..] row ]

directions :: [Direction]
directions = [\(x, y) -> (x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

isXmas :: Grid -> Direction -> Coord -> Bool
isXmas grid direction origin 
    | directionDoesNotExceedBounds grid direction origin = go grid direction origin 'X'
    | otherwise = False
    where
        go :: Grid -> Direction -> Coord -> Char -> Bool
        go grid direction coord 'S' = grid M.! coord == 'S'
        go grid direction coord 'A' = (grid M.! coord == 'A') && go grid direction (direction coord) 'S' 
        go grid direction coord 'M' = (grid M.! coord == 'M') && go grid direction (direction coord) 'A' 
        go grid direction coord 'X' = (grid M.! coord == 'X') && go grid direction (direction coord) 'M' 

        directionDoesNotExceedBounds :: Grid -> Direction -> Coord -> Bool
        directionDoesNotExceedBounds grid direction origin = length (catMaybes [M.lookup (iterate direction origin !! j) grid | j <- [0 .. 3]]) == 4

findAllXmases :: Grid -> Int
findAllXmases grid = sum $ map (\x -> length $ filter (\y -> isXmas grid y (fst x)) directions) (M.toList grid)

partOne :: String -> Int
partOne input = findAllXmases $ readGrid (lines input)

isMas :: [[Char]] -> Bool
isMas [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = True
isMas [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = True
isMas [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = True
isMas [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = True
isMas _ = False

findAllMases :: Grid -> Int -> Int
findAllMases grid bound = length $ filter (==True) [isMas [[grid M.! (x + i, y + j) | i <- [0 .. 2]] | j <- [0 .. 2]] | x <- [0 .. bound], y <- [0 .. bound]]

partTwo :: String -> Int
partTwo input = findAllMases (readGrid (lines input)) ((-) (length $ head (lines input)) 3)
