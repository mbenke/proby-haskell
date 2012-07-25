module Cfg.Simple.TestSelectSetR where
import Cfg.Simple
import Cfg.Simple.SelectSetR

import Test.HUnit
import qualified Data.Set as Set

run = runTestTT tests
tests = TestList [TestLabel "nullable" t1, "first_g1" ~: t2,t3]
t1 :: Test
t1 = (nullable_nt g1 $ NT 'L') ~? "g1: L nullable"

e2 = (first_nt g1 $ NT 'L')

t2 = Set.fromList [EOT,T 'a'] ~=? (first_nt g1 $ NT 'L')
t3 = Set.fromList [EOT,T 'a'] ~=? first g1 [NT 'L']

t4 =  nullable_nt g2 (NT 'E') -- False
