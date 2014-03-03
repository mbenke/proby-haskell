import Test.QuickCheck
import Control.Monad(liftM3)
import Data.List(sort)

import UTree

main = test
test = do
     writeln "Test fromList"
     quickCheck propFromListBalanced
     quickCheck propFromListBag
     writeln "Test maxdepth"
     quickCheck propMaxDepth

propFromListBag :: [Int] -> Bool
propFromListBag xs = sort xs == toList (fromList xs)

propFromListBalanced :: [Int] -> Bool
propFromListBalanced xs = balanced $ fromList xs

propMaxDepth :: [Int] -> Bool
propMaxDepth xs = depth (fromList xs) <= n+1 where n = log2(length xs)

log2 :: Integral a => a -> a
log2 n | n < 0 = error "log2: negative argument" 
       | n < 2 = 0
       | otherwise = 1 + log2 (div n 2)

write = putStr
writeln = putStrLn
