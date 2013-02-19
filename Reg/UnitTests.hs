import Mon
import Reg
import RegExtra0

import Test.HUnit

main = runUnitTests

testRe1 = Many digit <> string "ala"
testRe2 = Many digit <> Many letter <> Many digit

testStr1 = replicate 1000 '0' ++ "ala"

runUnitTests = runTestTT tests
tests = TestList 
  [ accepts testRe1 testStr1 ~? "testRe1" 
  , accepts testRe2 testStr1 ~? "testRe2"
  ]