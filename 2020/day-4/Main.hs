module Main where

import qualified Data.Map as M (Map, empty, fromList, member, lookup) 
import Data.List.Split
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA
import System.IO
import Control.Monad


replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace old new (x:xs)
  | x == old = [new] ++ (replace old new xs)
  | otherwise = [x] ++ (replace old new xs)

replaceNewlines :: String -> String
replaceNewlines [] = []
replaceNewlines s = replace '\n' ' ' s

separateBlankLines :: String -> [String]
separateBlankLines s = splitOn "\n\n" s

parseKeyValue :: String -> Maybe (String, String)
parseKeyValue s
  | length kv == 2 = Just (kv!!0, kv!!1)
  | otherwise = Nothing
  where kv = splitOn ":" s

filterPairs :: [Maybe (String, String)] -> [(String, String)]
filterPairs [] = []
filterPairs (x:xs) = case x of
  Just pair -> [pair] ++ (filterPairs xs)
  Nothing   -> filterPairs xs

type Passport = M.Map String String

parsePassport :: String -> Passport
parsePassport s = M.fromList $ filterPairs $ map parseKeyValue $ splitOn " " s

parseInput :: String -> [Passport]
parseInput s = map (parsePassport . replaceNewlines) $ separateBlankLines s

checkPassport :: Passport -> [String] -> Bool
checkPassport p [] = True
checkPassport p (x:xs)
  | M.member x p = checkPassport p xs
  | otherwise  = False

requiredFields :: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

checkRequiredFields :: Passport -> Bool
checkRequiredFields p = checkPassport p requiredFields

-- Part 1 main function
-- main :: IO ()
-- main = interact $ show . sum . map (fromEnum . checkRequiredFields) . parseInput

-- Extra segment for part 2

regexCheck :: String -> String -> Bool
regexCheck regex input = input =~ regex

checkRange :: (Int,Int) -> Int -> Bool
checkRange (min,max) num = num >= min && num <= max

regexCheckRange :: (Int, Int) -> String -> String -> Bool
regexCheckRange range regex input
  | match == input && (length sub) == 1 = checkRange range $ fromMaybe 0 (readMaybe $ head $ sub)
  | otherwise = False
  where (bef, match, aft, sub) = input =~ regex :: (String, String, String, [String])

regexCheckHeight :: String -> Bool
regexCheckHeight input
  | unit == "cm" = regexCheckRange (150,193) "^([0-9]{3})cm$" input
  | unit == "in" = regexCheckRange (59,76)   "^([0-9]{2})in$" input
  | otherwise = False
  where (num, unit) = ((init $ init $ input), ([last $ init $ input] ++ [last $ input]))

type FieldCheck = String -> Bool

requiredFieldChecks :: [(String, FieldCheck)]
requiredFieldChecks = 
  [("byr", (regexCheckRange (1920,2002) "^([0-9]{4})$")),
   ("iyr", (regexCheckRange (2010,2020) "^([0-9]{4})$")),
   ("eyr", (regexCheckRange (2020,2030) "^([0-9]{4})$")),
   ("hgt", regexCheckHeight),
   ("hcl", (regexCheck "^#[0-9a-f]{6}$")),
   ("ecl", (regexCheck "^(amb)?(blu)?(brn)?(gry)?(grn)?(hzl)?(oth)?$")),
   ("pid", (regexCheck "^[0-9]{9}$"))]

checkPassportDetail :: [(String, FieldCheck)] -> Passport -> Bool
checkPassportDetail [] p = True
checkPassportDetail ((key,check):xs) p = case (M.lookup key p) of
  Just value -> (check value) && (checkPassportDetail xs p)
  Nothing    -> False

execFromFile :: String -> IO ()
execFromFile filename = print . show . sum . map (fromEnum . (checkPassportDetail requiredFieldChecks)) . parseInput =<< readFile filename

main :: IO ()
main = interact $ show . sum . map (fromEnum . (checkPassportDetail requiredFieldChecks)) . parseInput
