import Data.List

f1::Int->[String]->Int
f1 _ [] = 0
f1 pos (row:rest) = (if (row !! (pos `mod` (length row))) == '#' then 1 else 0) + f (pos + 3) rest
