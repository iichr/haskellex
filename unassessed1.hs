-- import Data.List (delete)

f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
    where
        ys = [a | a <- xs, a<=x]
        zs = [a | a <- xs, a>x]

removeElem y xs = [x | x <-xs, x /= y]

permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (removeElem x xs)]
