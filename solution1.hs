import Data.List

f::[Integer] -> Integer 
f xs = head [x*y| (x:rest) <- tails xs, y <- rest, x + y == 2020]
