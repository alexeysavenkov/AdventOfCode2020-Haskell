import Data.List
import Data.Maybe

split_::Eq a => [a] -> a -> [a] -> [[a]]
split_ accum _ [] = [reverse accum]
split_ accum delim (x:xs) = if delim == x then (reverse accum):(split_ [] delim xs) else split_ (x:accum) delim xs

split::Eq a => a -> [a] -> [[a]]
split = split_ []

f::[[(String,String)]] -> Int
f lines = 
    let passports = map concat $ split [] lines
    in length $ filter (\passport -> all (\field -> isJust $ lookup field passport) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) passports
