module Main where
import Data.List (sort, nub)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

-- Polymer type definitions
type Element = Char
type Polymer = String

type PairRule = ((Char, Char), Char)

readPairRule :: String -> Maybe PairRule
readPairRule s
    | l == 2 && length ab == 2 && length c == 1 = Just ((a, b), head c)
    | otherwise = Nothing
  where parts = splitOn " -> " s
        l = length parts
        ab = head parts
        a = head ab
        b = last ab
        c = last parts


-- Polymer insertion logic
checkInsertRule :: (Element, Element) -> PairRule -> Maybe Element
checkInsertRule (a, b) rule
    | a == fst pair && b == snd pair = Just (snd rule)
    | otherwise = Nothing
  where pair = fst rule

applyInsertRules :: (Element, Element) -> [PairRule] -> Maybe Element
applyInsertRules (a,b) [] = Nothing
applyInsertRules (a,b) (r:rest)
    | isJust check = check
    | otherwise    = applyInsertRules (a,b) rest
  where check = checkInsertRule (a,b) r

applyInsertRulesToAll :: Polymer -> [PairRule] -> Polymer
applyInsertRulesToAll [] _ = []
applyInsertRulesToAll [a] _ = [a]
applyInsertRulesToAll (a:b:rest) r
    | isJust insert = [a] ++ [fromJust insert] ++ rest_polymer
    | otherwise     = a : rest_polymer
  where insert = applyInsertRules (a,b) r
        rest_polymer = applyInsertRulesToAll (b:rest) r

runPolymerSteps :: Int -> Polymer -> [PairRule] -> Polymer
runPolymerSteps 0 p _ = p
runPolymerSteps x p r = runPolymerSteps (x-1) next r
  where next = applyInsertRulesToAll p r


-- Read Input helpers
readInputLines :: [String] -> Maybe (Polymer, [PairRule])
readInputLines ls
    | empty == "" && isJust rules = Just (template, fromJust rules)
    | otherwise   = Nothing
  where template = head ls
        empty = head $ tail ls
        rest = tail $ tail ls
        rules = mapM readPairRule rest

readInput :: String -> Maybe (Polymer, [PairRule])
readInput s = readInputLines $ lines s


-- Polymer progression functions
countElement :: Polymer -> Element -> (Int, Polymer)
countElement [] _ = (0, [])
countElement (a:rest) e
    | a == e = (fst res + 1, snd res)
    | otherwise = (0, a:rest)
  where res = countElement rest e

elementQuantities :: Polymer -> [(Element, Int)]
elementQuantities [] = []
elementQuantities p = (e, count) : elementQuantities rest
  where e = head p
        t = countElement p e
        count = fst t
        rest = snd t

extremeQuantities :: Polymer -> (Int, Int)
extremeQuantities p = (head qs, last qs)
  where qs = sort $ map snd $ elementQuantities $ sort p


-- Solve Results
firstResult :: (Polymer, [PairRule]) -> Int
firstResult input = snd minmax - fst minmax
  where p = uncurry (runPolymerSteps 10) input
        minmax = extremeQuantities p

firstDisplay :: Int -> String
firstDisplay num = "Part One: What do you get if you take the quantity of the most common element and subtract the quantity of the least common element? - " ++ show num

firstSolve :: (Polymer, [PairRule]) -> String
firstSolve input = firstDisplay $ firstResult input

secondResult :: (Polymer, [PairRule]) -> Int
secondResult input = snd minmax - fst minmax
  where p = uncurry (runPolymerSteps 40) input
        minmax = extremeQuantities p

secondDisplay :: Int -> String
secondDisplay num = "Part Two: What do you get if you take the quantity of the most common element and subtract the quantity of the least common element? - " ++ show num

secondSolve :: (Polymer, [PairRule]) -> String
secondSolve input = secondDisplay $ secondResult input

processBoth :: (Polymer, [PairRule]) -> String
processBoth input = firstSolve input ++ "\n\n" ++ secondSolve input

solve :: String -> String
solve s = maybe "Invalid Input!" processBoth (readInput s)

main :: IO ()
main = interact solve
