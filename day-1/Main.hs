module Main where


findMatching :: Int -> Int -> [Int] -> Maybe Int
findMatching target a [] = Nothing
findMatching target a (x:xs)
  | a + x == target = Just x
  | otherwise       = findMatching target a xs


getMatching :: [Int] -> Int -> Maybe (Int, Int)
getMatching [] x = Nothing
getMatching (a:[]) x = Nothing
getMatching (x:xs) target = case res of
    Just other -> Just (x, other)
    Nothing    -> getMatching xs target
  where res = findMatching target x xs


wrapChallenge :: [Int] -> String
wrapChallenge nums = case result of
    Just (a, b) -> show $ a * b
    Nothing  -> "No two inputs resulted in 2020!"
  where result = getMatching nums 2020


findThreeMatching :: Int -> Int -> [Int] -> Maybe (Int, Int)
findThreeMatching target a vals = getMatching vals (target - a)


getThreeMatching :: [Int] -> Int -> Maybe (Int, Int, Int)
getThreeMatching [] target = Nothing
getThreeMatching (a:[]) target = Nothing
getThreeMatching (x:xs) target = case res of
    Just (a, b) -> Just (x, a, b)
    Nothing     -> getThreeMatching xs target
  where res = findThreeMatching target x xs

wrapThreeChallenge :: [Int] -> String
wrapThreeChallenge nums = case result of
    Just (a, b, c) -> show $ a * b * c
    Nothing -> "No three inputs resulted in 2020!"
  where result = getThreeMatching nums 2020

main :: IO ()
main = interact $ wrapThreeChallenge . map read . lines
