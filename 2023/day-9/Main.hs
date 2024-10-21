module Main where

parseLine :: String -> [Int]
parseLine s = map read (words s)

diffSteps :: [Int] -> [Int]
diffSteps [] = []
diffSteps (x0:xs)
    | length xs == 0 = []
    | otherwise        = [(x1 - x0)] ++ (diffSteps xs)
    where
    x1 = head $ xs

stepsEqual :: [Int] -> Bool
stepsEqual [] = False
stepsEqual steps
    | length steps < 2 = False
    | s0 == s1 = stepsEqual (tail steps)
    | otherwise = False
    where
    s0 = head steps
    s1 = head $ tail steps

type DiffList = [[Int]]

nextDiffList :: DiffList -> DiffList
nextDiffList dl
    | stepsEqual ll = dl
    | otherwise     = dl ++ (nextDiffList [(diffSteps ll)])
    where
    ll = last dl

parseDiffList :: String -> DiffList
parseDiffList s = nextDiffList $ [(parseLine s)]

-- param xs     reverse list of last values of each row
extrapolateSteps :: [Int] -> Int -> [Int]
extrapolateSteps [] _ = []
extrapolateSteps (x:xs) i = [y] ++ (extrapolateSteps xs y)
    where y = x + i

appendExtrapolation :: [Int] -> Int -> [Int]
appendExtrapolation xs x = xs ++ [x]

appendExtrapolations :: DiffList -> [Int] -> DiffList
appendExtrapolations [] _ = []
appendExtrapolations _ [] = []
appendExtrapolations dl inc = [(appendExtrapolation (head dl) (head inc))] ++ (appendExtrapolations (tail dl) (tail inc))

extrapolateDiffList :: DiffList -> DiffList
extrapolateDiffList dl = appendExtrapolations dl inc
    where
    lasts = reverse $ map last dl
    inc = reverse $ extrapolateSteps lasts 0

handleLine :: String -> String
handleLine s = show $ extrapolateDiffList $ parseDiffList s

parseInput :: String -> String
parseInput s = unlines $ map handleLine (lines s)

main :: IO ()
main = interact parseInput
