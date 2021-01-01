import Data.List

between::Ord a => a -> (a, a) -> Bool
between x (mn,mx) = x >= mn && x <= mx 

f::[(Int, Int, Char, String)] -> Int
f input = length $ filter (\(mn,mx,ch,str) -> ((length . filter (== ch)) str) `between` (mn, mx)) input
