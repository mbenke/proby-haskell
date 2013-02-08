{-# LANGUAGE NoMonomorphismRestriction #-}
import Test.QuickCheck
import Control.Monad(liftM2)
import Data.Functor((<$>))

import Mon
import Reg
import RegExtra
  
leftUnit :: Reg AB -> Bool
leftUnit x = m1 <> x == x

rightUnit :: Reg AB -> Bool
rightUnit x =  x <> m1 == x

assoc :: Reg AB -> Reg AB -> Reg AB -> Bool
assoc x y z = (x<>y)<>z == x<>(y<>z)

nullableUnit :: Bool
nullableUnit = nullable m1

-- nullableOp :: Reg AB -> Property
nullableOp = forAllNullable $ \x -> forAllNullable $ \y ->  nullable (x <> y)

iff :: Bool -> Bool -> Bool
iff a b = (a && b) || (not a && not b)

nullableSimpl, emptySimpl :: Reg AB -> Bool
nullableSimpl x = nullable x `iff` nullable (simpl x)
emptySimpl x = empty x `iff` empty (simpl x)

recLeftNul :: Reg AB -> Property
recLeftNul y = forAllNullable $ \x ->  
               forAllMatching y $ \cs -> 
               accepts y cs ==> accepts (x:>y) cs

recRightNul :: [AB] -> Reg AB -> Property
recRightNul cs x = forAllNullable $ \y ->  
               -- forAllMatching x $ \cs -> 
               accepts x cs ==> accepts (x:>y) cs

writeln = putStrLn

main = do
       writeln "testing left unit"
       quickCheck leftUnit
       writeln "testing right unit"
       quickCheck rightUnit
       writeln "assoc"
       quickCheck assoc
       print "nullable unit"
       quickCheck nullableUnit
       print "nullable op"
       quickCheck nullableOp
       writeln "nullableSimpl"
       quickCheck nullableSimpl
       quickCheck emptySimpl

       --writeln "cs ∈ L(y) && ε ∈ L(x) ==> cs ∈ L(x:>y)"
       --quickCheck recLeftNul
       --writeln "cs ∈ L(x) && ε ∈ L(y) ==> cs ∈ L(x:>y)"
       --quickCheck recRightNul

------------------------------------------------------------
-- Auxilliary       
------------------------------------------------------------
       
instance Arbitrary AB where
  arbitrary = oneof [return A, return B]
  
shrinkReg :: Eq c => Reg c -> [Reg c]
shrinkReg r = if r == s then [] else [s] where s = simpl r 

--liftR f x = liftM2 f x x

instance (Eq c,Arbitrary c) => Arbitrary (Reg c) where
  arbitrary = sized arb where
    arb 0 = oneof [return Eps, return Empty]
    arb 1 = Lit <$> arbitrary
    arb n = oneof [Many <$> arb2, liftM2 (:>) arb2 arb2, liftM2 (:|) arb2 arb2] where
      arb1 = arb (n-1) 
      arb2 = arb (n `div` 2)

  shrink = shrinkReg            
       
forAllNullable :: (Testable prop) => (Reg AB -> prop) -> Property
forAllNullable = forAll genNullableAB

genNullableAB :: Gen (Reg AB)
genNullableAB = genNullable

genNullable :: Gen (Reg AB)
genNullable = sized gn where
  gn 0 = return Eps
  gn n = oneof [
    Many <$> arbitrary, 
    liftM2 (:>) gn2 gn2,
    liftM2 (:|) arbitrary gn2,
    liftM2 (:|) gn2 arbitrary] where
      gn2 = gn (n `div` 2)
      
forAllMatching = forAll . genMatching
genMatching :: Reg AB -> Gen [AB]
genMatching r = sized (gm r)  `suchThat` accepts r 

gm r 0 | nullable r = return []
gm r n | n < 0 = return []
       | otherwise = oneof $ [liftM2 (:) gc gcs |
                             (gc,gcs) <- [(return c,gm (der c r) (n-1)) |
                             c  <- [A,B], mayStart c r]] `whenEmpty` [return []]

whenEmpty [] d = d
whenEmpty a d = a
                             
{-
gm r 0 | nullable r = return []
       | otherwise = elements [[A],[B]]
gm r n = gmn (simpl r) where
    gmn (Lit c) = return [c]
    gmn (r1 :| r2) = oneof [gmn r1, gmn r2]
    gmn (Lit c:> r) = do { cs <- gm r (n-1); return (c:cs) }
    gmn (r1 :> r2) = do
      k <- choose (0,n) 
      liftM2 (++) (gm r1 k) (gm r2 (n-k))
    gmn (Many r) = do 
      k <- choose (0,n) 
      if k == 0 then return [] else splitAt (n-k) (gm r) (gm (Many r))
    gmn _ = return []
    splitAt k g1 g2 = 
      liftM2 (++) (g1 k) (g2 (n-k))
-}
genRegAB :: Gen (Reg AB)
genRegAB = arbitrary
