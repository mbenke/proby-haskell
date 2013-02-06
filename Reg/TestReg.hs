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
    arb n = oneof [Many <$> arb1, liftM2 (:>) arb2 arb2, liftM2 (:|) arb2 arb2] where
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