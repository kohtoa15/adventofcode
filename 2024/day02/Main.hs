#!/usr/bin/env stack
-- stack script --snapshot lts-22.28
import System.Environment (getArgs)

type IntPair = (Int, Int)

pairs :: [Int] -> [IntPair]
pairs xs
    | length xs < 2 = []
    | otherwise     = [((xs !! 0), (xs !! 1))] ++ pairs (tail xs)

diff :: IntPair -> Int
diff (a,b) = abs $ a - b

isLessThan :: Int -> Int -> Bool
isLessThan cond val = val < cond

maxDiff3 :: [IntPair] -> Bool
maxDiff3 xs = all (isLessThan 4) $ map diff xs

increasing :: [IntPair] -> Bool
increasing xs = all (uncurry (>)) xs

decreasing :: [IntPair] -> Bool
decreasing xs = all (uncurry (<)) xs

isSafe :: [Int] -> Bool
isSafe xs = (increasing ps || decreasing ps) && maxDiff3 ps
    where
    ps = pairs xs

removeLvlN :: [Int] -> Int -> [Int]
removeLvlN xs n
    | length xs == 0 = []
    | n == 0 = tail xs
    | otherwise = [head xs] ++ (removeLvlN (tail xs) (n-1))

dampener :: [Int] -> Bool
dampener xs = (isSafe xs) || (any isSafe removedLevel)
    where
    removedLevel = map (uncurry removeLvlN) $ zip (repeat xs) [0..(length xs)]

levels :: String -> [Int]
levels line = map read (words line)

solve :: String -> ([Int] -> Bool) -> String
solve l filFn = show $ length $ filter filFn $ map levels $ lines $ l

execPartN :: String -> ([Int] -> Bool)
execPartN n
    | n == "part1" = isSafe
    | n == "part2" = dampener
    | otherwise = error "invalid part selected"

main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    putStrLn $ solve file (execPartN (args !! 1))
