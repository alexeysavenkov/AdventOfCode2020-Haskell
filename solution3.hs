import Data.List

f::Int->Int->Int->[String]->Int
f _ _ _ [] = 0
f slopex slopey pos (row:rest) = (if (row !! (pos `mod` (length row))) == '#' then 1 else 0) + f slopex slopey (pos + slopex) (drop (slopey - 1) rest)

f2::[String]->Integer
f2 m = product $ map (\(x,y) -> fromIntegral $ f x y 0 m) [(1,1),(3,1),(5,1),(7,1),(1,2)]
