import Mon
import Reg
import RegExtra

main = runSimpleTests

testRe1 = Many digit <> string "ala"
testRe2 = Many digit <> Many letter <> Many digit

testStr1 = replicate 1000 '0' ++ "ala"

runSimpleTests = runTests pairs where
     values = [s $ accepts testRe1 testStr1]
     verify = [s True]
     pairs = zip values verify
     -- pairs =
     s x = show x -- cannot eta (MR?)
     runTests = mapM_ runTest
     runTest (a, b) =
        print(if a == b
                then "PASS"
                else "FAIL. Expected: " ++ b ++ " Actual: " ++ a)



 