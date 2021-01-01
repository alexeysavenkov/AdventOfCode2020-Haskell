import Data.List

f1::[Integer] -> Integer
f1 xs = head [x*y| (x:rest) <- tails xs, y <- rest, x + y == 2020]

f2::[Integer] -> Integer
f2 xs = head [x*y*z| (x:rest) <- tails xs, (y:rest2) <- tails rest, z <- rest2, x + y + z == 2020]
