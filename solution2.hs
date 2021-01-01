import Data.List

between::Ord a => a -> (a, a) -> Bool
between x (mn,mx) = x >= mn && x <= mx 

f::[(Int, Int, Char, String)] -> Int
f1 input = length $ filter (\(mn,mx,ch,str) -> ((length . filter (== ch)) str) `between` (mn, mx)) input

get::[a] -> Int -> Maybe a
get xs i = if i < length xs then Just (xs !! i) else Nothing

f2::[(Int, Int, Char, String)] -> Int
f2 input = length $ filter (\(mn,mx,ch,str) -> (length . filter (==(Just ch)) $ [get str (mn-1), get str (mx-1)] ) == 1) input
