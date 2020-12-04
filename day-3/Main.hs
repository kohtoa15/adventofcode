module Main where

parseLandscape :: Char -> Maybe Int
parseLandscape '#' = Just 1
parseLandscape '.' = Just 0
parseLandscape _   = Nothing

parseLine :: String -> Maybe [Int]
parseLine [] = Just []
parseLine (x:xs) = case (parseLandscape x) of
  Just i -> case (parseLine xs) of
    Just rest -> Just ([i] ++ rest)
    Nothing   -> Nothing
  Nothing -> Nothing

parseInput :: [String] -> Maybe [[Int]]
parseInput [] = Just []
parseInput (x:xs) = case (parseLine x) of
  Just line -> case (parseInput xs) of
    Just rest -> Just ([line] ++ rest)
    Nothing   -> Nothing
  Nothing -> Nothing

getElem :: [a] -> Int -> a
getElem list i
  | i >= len = getElem list (i `mod` len)
  | otherwise = list!!i
  where len = length list

getValueAt :: [[Int]] -> Int -> Int -> Int
getValueAt grid x y = getElem (getElem grid y) x

slideInterval :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int -> Int
slideInterval grid (x,y) (dx,dy) max
  | y < max = (getValueAt grid x y) + (slideInterval grid ((x + dx),(y + dy)) (dx,dy) max)
  | otherwise = 0

slideDownSlope :: [[Int]] -> (Int, Int) -> Int
slideDownSlope grid delta = slideInterval grid (0,0) delta max
  where max = length grid

multiplySlopes :: [[Int]] -> [(Int, Int)] -> Int
multiplySlopes grid [] = 1
multiplySlopes grid (d:ds) = (slideDownSlope grid d) * (multiplySlopes grid ds)

maybeSlide :: Maybe [[Int]] -> String
maybeSlide input = case input of
  Just grid -> show $ multiplySlopes grid [(1,1), (3,1), (5,1), (7,1), (1,2)]
  Nothing   -> "Invalid input!"

main :: IO ()
main = interact $ maybeSlide . parseInput . lines
