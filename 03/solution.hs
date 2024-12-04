import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

data Term = Mul Int Int 
            | Junk Char
            | Do
            | Dont
            deriving (Show, Eq)

mulParser :: ReadP Term 
mulParser = do
    string "mul("
    a <- munch1 isDigit
    string ","
    b <- munch1 isDigit
    string ")"
    return (Mul (read a) (read b))

doParser :: ReadP Term 
doParser = do
    string "do()"
    return Do

dontParser :: ReadP Term 
dontParser = do
    string "don't()"
    return Dont

junkParser :: ReadP Term
junkParser = do Junk <$> get

termParser :: ReadP [Term]
termParser = many1 $ choice [mulParser, doParser, dontParser] <++ junkParser

parseInput :: String -> [Term]
parseInput input = fst $ last $ readP_to_S termParser input

partOne :: String -> Int
partOne input = sum $ map evaluateTerm $ parseInput input
    where
        evaluateTerm :: Term -> Int
        evaluateTerm (Mul a b) = a * b
        evaluateTerm _ = 0

partTwo :: String -> Int
partTwo input = evaluateTerms $ parseInput input
    where
        evaluateTerms :: [Term] -> Int
        evaluateTerms [] = 0
        evaluateTerms ((Mul a b):ts) = a * b + evaluateTerms ts
        evaluateTerms ((Junk a):ts) = evaluateTerms ts
        evaluateTerms (Dont:ts) = evaluateTerms $ dropWhile (/= Do) ts
        evaluateTerms (Do:ts) = evaluateTerms ts

