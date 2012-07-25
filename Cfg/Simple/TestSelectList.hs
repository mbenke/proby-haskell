module Cfg.Simple.TestSelectList where
import Cfg.Simple
import Cfg.Simple.SelectList

import Test.HUnit

run = runTestTT tests
tests = TestList [TestLabel "nullable" t1, "first_g1" ~: t2,t3]
t1 :: Test
t1 = (nullable_nt g1 $ NT 'L') ~? "g1: L nullable"
t2 = [EOT,T 'a'] ~=? (first_nt g1 $ NT 'L')
t3 = [EOT,T 'a'] ~=? first g1 [NT 'L']

t4 =  nullable_nt g2 (NT 'E') -- False
