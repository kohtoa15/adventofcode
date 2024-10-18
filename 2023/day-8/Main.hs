import Data.Maybe (isJust, fromJust)

-- function lookupPattern
-- Finds the index of the first occurrence of a pattern String within another String source.
-- param String source          where we want to find the pattern
-- param String pattern         the pattern we want to find whole in source
-- param Int index              counter to specify already processed chars
-- returns Nothing when pattern is not in source, and the index of the first occurrence otherwise
_lookupPattern :: String -> String -> Int -> Int -> Maybe Int
_lookupPattern (s:srest) (p:prest) cur_index match_since
    | s == p    = _lookupPattern srest prest (cur_index+1) match_since  -- test the next char and increase counter
    | otherwise = _lookupPattern srest (p:prest) (cur_index+1) (cur_index+1) -- test the next source char
_lookupPattern s "" i m = Just m  -- pattern is exhausted, so has been found
_lookupPattern "" _ _ _ = Nothing  -- empty source (and non-empty pattern) means pattern did not fit
lookupPattern :: String -> String -> Maybe Int
lookupPattern s p = _lookupPattern s p 0 0

splitAtPattern :: String -> String -> (String, String)
splitAtPattern source pattern = (splitFront, splitBack)
    where
    cutoff = fromJust $ lookupPattern source pattern
    firstSplit = splitAt cutoff source
    splitFront = fst $ firstSplit
    splitBack = snd $ splitAt (length pattern) (snd $ firstSplit)

type NodeRelation = (String, (String, String))

parseNodes :: String -> NodeRelation
parseNodes line = ((fst split_equals), (pair_fst, pair_snd))
    where
    split_equals = splitAtPattern line " = "
    split_pair = splitAtPattern (snd split_equals) ", "
    pair_fst = snd $ splitAtPattern (fst split_pair) "("
    pair_snd = fst $ splitAtPattern (snd split_pair) ")"

formatNode :: NodeRelation -> String
formatNode nr = (fst nr) ++ " <=> " ++ (fst $ snd nr) ++ "," ++ (snd $ snd nr)

parseContent :: [String] -> (String, [NodeRelation])
parseContent (x:xs) = (x, relations)
    where relations = map parseNodes $ tail xs

singleNavigate :: Char -> (String, String) -> Maybe String
singleNavigate 'R' (l, r) = Just r
singleNavigate 'L' (l, r) = Just l
singleNavigate _ _ = Nothing

matchNode :: String -> NodeRelation -> Maybe (String, String)
matchNode name (rel_name, pair)
    | name == rel_name = Just pair
    | otherwise        = Nothing

findNode :: String -> [NodeRelation] -> Maybe (String, String)
findNode _ [] = Nothing
findNode name (r:rest)
    | isJust try_match = try_match
    | otherwise        = findNode name rest
    where try_match = matchNode name r

nextNode :: String -> [NodeRelation] -> Char -> String
nextNode name rel nav = fromJust $ singleNavigate nav next_pair
    where next_pair = fromJust $ findNode name rel

navigate :: String -> String -> String -> [NodeRelation] -> [String]
navigate nav name end rel 
    | name == end = [name]
    | otherwise = [name] ++ find_next_node
    where
    find_next_node = navigate next_nav (nextNode name rel (head nav)) end rel
    next_nav = (tail nav) ++ [(head nav)]

navigateReport :: [String] -> String
navigateReport n = unwords $ n ++ [result]
    where
    steps = (length n) - 1
    result = "\nSteps: " ++ (show steps)

-- param s:     complete stdin string
readInput :: String -> String
readInput s = navigateReport $ navigate path "AAA" "ZZZ" rel
    where
    content = parseContent $ lines s
    path = fst content
    rel = snd content

main :: IO ()
main = interact readInput
