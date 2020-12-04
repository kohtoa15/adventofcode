module Main where
import Data.List.Split

toTuple :: [a] -> (a, a)
toTuple (a:b:rest) = (a, b)

tupleSplit :: Eq a => [a] -> [a] -> ([a], [a])
tupleSplit a b = toTuple $ splitOn a b

third :: (a, b, c) -> c
third (a, b, c) = c

getRange :: String -> (Int, Int)
getRange input = (read $ fst $ x, read $ snd $ x)
  where x = tupleSplit "-" input

splitRangeChar :: String -> (String, String)
splitRangeChar input = tupleSplit " " input

splitPwdPolicy :: String -> (String, String)
splitPwdPolicy input = tupleSplit ": " input

parseInput :: String -> ((Int, Int), Char, String)
parseInput input =
  (getRange $ fst $ splitRangeChar $ lhs,
   head $ snd $ splitRangeChar $ lhs, 
   rhs)
  where (lhs, rhs) = splitPwdPolicy input

countOcrHelper :: Char -> String -> Int -> Int
countOcrHelper c [] cnt = cnt
countOcrHelper c (x:xs) cnt
  | c == x    = countOcrHelper c xs (cnt + 1)
  | otherwise = countOcrHelper c xs cnt

countOccurrences :: Char -> String -> Int
countOccurrences c s = countOcrHelper c s 0

checkPassword :: ((Int, Int), Char, String) -> Bool
checkPassword ((min, max), char, pwd) = count >= min && count <= max
  where count = countOccurrences char pwd

containsAt :: Int -> Char -> String -> Bool
containsAt n c [] = False
containsAt 1 c (x:xs) = c == x
containsAt n c (x:xs) = containsAt (n - 1) c xs

checkPassword2 :: ((Int, Int), Char, String) -> Bool
checkPassword2 ((a, b), char, pwd) = matchA /= matchB
  where
    matchA = containsAt a char pwd
    matchB = containsAt b char pwd

main :: IO ()
main = interact $ show . sum . map (fromEnum . checkPassword2 . parseInput) . lines
