-- mainmodule TestBST where
import Test.QuickCheck
import Control.Monad(liftM3)
import Data.List(sort)

import BST

main = test
test = do
     writeln "Rotations:"
     quickCheck propRot1LSeq
     quickCheck propRot1RSeq
     quickCheck propRot2LSeq
     quickCheck propRot2RSeq
     writeln "Test fromList"
     quickCheck propFromListBalanced
     quickCheck propFromListBag
     writeln "Test maxdepth"
     quickCheck propMaxDepth
     quickCheck propMinDepth

propMaxDepth :: [Int] -> Bool
propMaxDepth xs = depth (fromList xs) <= 2*n+1 where n = log2 (length xs)

propMinDepth :: [Int] -> Bool
propMinDepth xs = depth (fromList xs) >= n `div` 2 where n = log2 (length xs)

log2 :: Integral a => a -> a
log2 n | n < 0 = error "log2: negative argument" 
       | n < 2 = 0
       | otherwise = 1 + log2 (div n 2)

-- imLog :: Integer->Integer->Integer
imLog b x |  x < b = 0
 | otherwise = let
          l = 2 * imLog (b*b) x
          doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
        in
           doDiv (x`div`(b^l)) l
 
propRot1LSeq :: Tree Int -> Bool
propRot1LSeq t = toList t == toList (rot1L t)

propRot1RSeq :: Tree Int -> Bool
propRot1RSeq t = toList t == toList (rot1R t)

propRot2LSeq :: Tree Int -> Bool
propRot2LSeq t = toList t == toList (rot2L t)

propRot2RSeq :: Tree Int -> Bool
propRot2RSeq t = toList t == toList (rot2R t)

propFromListBag :: [Int] -> Bool
propFromListBag xs = sort xs == toList (fromList xs)

propFromListBalanced :: [Int] -> Bool
propFromListBalanced xs = balanced $ fromList xs

write = putStr
writeln = putStrLn

------------------------------------------------------------
-- Hic sunt leones
------------------------------------------------------------

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary  = sized tree where
    tree 0 = return Tip
    tree n = oneof [return Tip, liftM3 bin arbitrary subtree subtree] where
        subtree = tree (div n 2)

instance CoArbitrary a => CoArbitrary (Tree a) where
  coarbitrary Tip = variant 0
  coarbitrary (Bin _ x l r) = variant 1 . coarbitrary x . coarbitrary l . coarbitrary r
