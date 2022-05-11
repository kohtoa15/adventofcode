module Main where
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HM (empty, elems, adjust, fromList, toList, insert, lookup, insertWith, keys)
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

-- Mapping of known polymer patterns
data PatternSubSet = List [(Polymer, Polymer)] | Map (HashMap Polymer Polymer) deriving (Show)
type PatternSet = HashMap Int PatternSubSet
type PolymerInfo = (Polymer, PatternSet)

newSubSet :: Polymer -> Polymer -> PatternSubSet
newSubSet k v
    | length k > 1048576 = List [(k,v)]    -- avoid hashing large keys, when most likely we don't have many of the same size
    | otherwise       = Map (HM.insert k v HM.empty)

mergeSubSets :: PatternSubSet -> PatternSubSet -> PatternSubSet
mergeSubSets (List l1) (List l2) = List (l1 ++ l2)
mergeSubSets (List l) (Map m) = List (l ++ HM.toList m)
mergeSubSets (Map m) (List l) = List (l ++ HM.toList m)
mergeSubSets (Map m1) (Map m2) = Map (HM.insert k (fromJust $ HM.lookup k m1) m2)  -- we know that m1 can't be greater than length of 1
  where k = head $ HM.keys m1

patternInsert :: Polymer -> Polymer -> PatternSet -> PatternSet
patternInsert k v = HM.insertWith mergeSubSets l (newSubSet k v)
  where l = length k

subsetListLookup :: Polymer -> [(Polymer, Polymer)] -> Maybe Polymer
subsetListLookup p [] = Nothing
subsetListLookup p ((k,v):xs)
    | p == k    = Just v
    | otherwise = subsetListLookup p xs

subsetLookup :: Polymer -> PatternSubSet -> Maybe Polymer
subsetLookup p (Map m) = HM.lookup p m
subsetLookup p (List l) = subsetListLookup p l

patternLookup :: Polymer -> PatternSet -> Maybe Polymer
patternLookup k s = subsetLookup k =<< HM.lookup (length k) s

patternsFromRules :: [PairRule] -> PatternSet
patternsFromRules [] = HM.empty
patternsFromRules (r:rs) = patternInsert key val (patternsFromRules rs)
  where key = [a, b] -- start pattern is the two pair chars
        val = [a, c, b]  -- resulting pattern is with the resulting char in the middle
        a = fst $ fst r
        b = snd $ fst r
        c = snd r

polymerInfo :: (Polymer, [PairRule]) -> PolymerInfo
polymerInfo (p,r) = (p, patternsFromRules r)

-- tries to apply the patterns to the polymer and returns the result without updating the patterns
applyPatternSet :: Polymer -> PatternSet -> Polymer
applyPatternSet p pat
    | isJust result = fromJust result
    | otherwise     = p
  where result = patternLookup p pat

-- split polymer and progress with separate halves
splitPolymer :: PolymerInfo -> PolymerInfo
splitPolymer (p, pat) = (grownPolymer, patterns)
  where halves = splitAt (div (length p) 2) p
        insert = tail $ init $ applyPatternSet [last $ fst halves, head $ snd halves] pat
        grownFstHalf = growPolymer (fst halves, pat)
        grownSndHalf = growPolymer (snd halves, snd grownFstHalf)  -- use updated patterns
        grownPolymer = fst grownFstHalf ++ insert ++ fst grownSndHalf
        patterns = patternInsert p grownPolymer (snd grownSndHalf) -- add current whole as pattern

-- check options for undiscovered polymer
growUndiscovered :: PolymerInfo -> PolymerInfo
growUndiscovered (p, pat)
    | length p > 2 = splitPolymer (p, pat)   -- split if we have at least 3 elements
    | otherwise    = (p,pat)  -- on 2 elements, possible growths should already have been in patterns

-- check all polymer growth interactions and return updated polymer and patterns
growPolymer :: PolymerInfo -> PolymerInfo
growPolymer ([],pat) = ([], pat)
growPolymer ([x],pat) = ([x], pat)
growPolymer (p,pat)
    | isJust val = (fromJust val, pat)   -- if we know the pattern, just return it
    | otherwise  = growUndiscovered (p,pat) -- otherwise, split along the middle and search again
  where val = patternLookup p pat

growRepeatedly :: PolymerInfo -> Int -> PolymerInfo
growRepeatedly p 0 = p
growRepeatedly p x = growRepeatedly (growPolymer p) (x - 1)

solveHelperPat :: Int -> (Polymer, [PairRule]) -> PolymerInfo
solveHelperPat i (p, r) = growRepeatedly (p, patternsFromRules r) i


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
