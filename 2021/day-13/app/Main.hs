module Main where
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.List (nub, sortBy)
import Data.Foldable (Foldable(length))
import Data.Ord (Ordering)


-- Maybe Utils
unpackMaybeTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
unpackMaybeTuple (Just a, Just b) = Just (a, b)
unpackMaybeTuple _ = Nothing

concatMaybeLists :: Maybe [a] -> Maybe [a] -> Maybe [a]
concatMaybeLists (Just xs) (Just ns) = Just (xs ++ ns)
concatMaybeLists _ _ = Nothing

unpackMaybeList :: [Maybe a] -> Maybe [a]
unpackMaybeList [] = Just []
unpackMaybeList (Nothing:_) = Nothing
unpackMaybeList ((Just a):rest) = concatMaybeLists (Just [a]) (unpackMaybeList rest)


-- Vector type constructors and readers
type Vector = (Int, Int)

readVectorParts :: [String] -> Maybe Vector
readVectorParts [a, b] = unpackMaybeTuple(readMaybe a, readMaybe b)
readVectorParts _ = Nothing

readVector :: String -> Maybe Vector
readVector s = readVectorParts $ splitOn "," s


-- FoldInstr type constructors and readers
data FoldInstr = Horizontal Int | Vertical Int deriving(Show)

horizontal :: Int -> FoldInstr
horizontal = Horizontal

vertical :: Int -> FoldInstr
vertical = Vertical

readFoldInstrDef :: [String] -> Maybe FoldInstr
readFoldInstrDef [a, b]
    | a == "x" = (Just . horizontal) =<< readMaybe b
    | a == "y" = (Just . vertical) =<< readMaybe b
    | otherwise = Nothing
readFoldInstrDef _ = Nothing

readFoldInstrParts :: [String] -> Maybe FoldInstr
readFoldInstrParts [a, b, c]
    | a == "fold" && b == "along" = readFoldInstrDef $ splitOn "=" c
    | otherwise                   = Nothing
readFoldInstrParts _ = Nothing

readFoldInstr :: String -> Maybe FoldInstr
readFoldInstr s = readFoldInstrParts $ words s
        

-- Folding logic
foldValue :: Int -> Int -> Int
foldValue vec fold
    | vec > fold = 2 * fold - vec
    | otherwise  = vec

foldVector :: FoldInstr -> Vector -> Vector
foldVector (Vertical n) (x, y) = (x, foldValue y n)
foldVector (Horizontal n) (x, y) = (foldValue x n, y)

foldStep :: [Vector] -> FoldInstr -> [Vector]
foldStep vs i = map (foldVector i) vs

foldAll :: [Vector] -> [FoldInstr] -> [Vector]
foldAll = foldl foldStep

foldFirst :: [Vector] -> [FoldInstr] -> [Vector]
foldFirst vs (i:is) = foldStep vs i


-- Input handling helpers
splitFirstEmptyLine :: [String] -> ([String], [String])
splitFirstEmptyLine [] = ([], [])
splitFirstEmptyLine (x:xs)
    | x == ""   = ([], xs)
    | otherwise = (a, b)
  where t = splitFirstEmptyLine xs
        a = x : fst t
        b = snd t

readInput :: String -> Maybe ([Vector], [FoldInstr])
readInput s = unpackMaybeTuple (vectors, instrs)
  where input = splitFirstEmptyLine $ lines s
        vectors = unpackMaybeList $ map readVector (fst input)
        instrs = unpackMaybeList $ map readFoldInstr (snd input)


-- Solve results
-- Q1
firstResult :: ([Vector], [FoldInstr]) -> Int
firstResult = length . nub . uncurry foldFirst

showFirst :: Int -> String
showFirst i = "Part One: How many dots are visible after completing just the first fold instruction on your transparent paper? - " ++ show i ++ "\n"

processFirst :: ([Vector], [FoldInstr]) -> String
processFirst i = showFirst $ firstResult i

-- Q2
cmpPrioY :: Vector -> Vector -> Ordering
cmpPrioY a b
    | snd a == snd b = compare (fst a) (fst b)
    | otherwise      = compare (snd a) (snd b)

sortPrioY :: [Vector] -> [Vector]
sortPrioY = sortBy cmpPrioY

secondResult :: ([Vector], [FoldInstr]) -> [Vector]
secondResult = sortPrioY . nub . uncurry foldAll

transparent :: Int -> String
transparent 0 = ""
transparent x = "  " ++ transparent (x-1)

printVectors :: [Vector] -> Int -> Int -> String
printVectors [] _ _ = "\n"
printVectors (v:rest) row col
    | vrow > row = "\n" ++ printVectors (v:rest) vrow 0
    | vcol > col = transparent (vcol - col) ++ printVectors (v:rest) row vcol
    | otherwise  = "##" ++ printVectors rest row (col+1)
  where vrow = snd v
        vcol = fst v

printVectorSet :: [Vector] -> String
printVectorSet vs = printVectors vs 0 0

showSecond :: String -> String
showSecond s = "Part Two: What code do you use to activate the infrared thermal imaging camera system?\n\n" ++ s 

processSecond :: ([Vector], [FoldInstr]) -> String
processSecond input = showSecond $ printVectorSet $ secondResult input

showSolutions :: ([Vector], [FoldInstr]) -> String
showSolutions input = processFirst input ++ "\n" ++ processSecond input

solve :: String -> String
solve s = maybe "Invalid Input!" showSolutions (readInput s)

main :: IO ()
main = interact solve
