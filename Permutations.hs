module Permutations where

partitions1 :: [a] -> [([a],[a])]
partitions1 [] = []
partitions1 (x:xs) = ([x],xs) : [ (x:ys, zs) | (ys,zs) <- partitions1 xs]

-- For a list xs, list of all pairs (ys,zs) st xs == ys ++ zs
partitions :: [a] -> [([a],[a])]
partitions [] = [([],[])]
partitions (x:xs) = ([],x:xs) : [ (x:ys, zs) | (ys,zs) <- partitions xs]

inserts :: a -> [a] -> [[a]]
inserts x xs = [ ys ++ [x] ++ zs | (ys,zs) <- partitions xs]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [(inserts x) ys | ys <- permutations xs]
