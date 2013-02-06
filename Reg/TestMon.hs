import Mon
import Test.QuickCheck

left_unit :: [()] -> Bool
left_unit x = m1 <> x == x

main = do
       quickCheck left_unit