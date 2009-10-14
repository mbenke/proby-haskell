triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z) | (x,y,z) <- triples2 n, pyth (x,y,z), coprime23 x y z]

triples :: Int -> [(Int,Int,Int)]
triples n = [(x,y,z) | x <- [1..n], y <- [1..n], z <-[1..n]]

triples2 :: Int -> [(Int,Int,Int)]
triples2 n = [(x,y,z) | x <- [1..n], y <- [x..n], z <-[y..n]]

pyth :: (Int,Int,Int) -> Bool
pyth (x,y,z) = z^2 == x^2 + y^2

ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending [x] = True
ascending (x:xs@(y:ys)) = x<=y && ascending xs

coprime x y = 1 == gcd x y
coprime23 x y z = coprime x y || coprime y z || coprime x z
