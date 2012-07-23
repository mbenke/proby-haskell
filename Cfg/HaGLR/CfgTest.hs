import Cfg.HaGLR.Cfg
import Data.Char(isUpper)

type Sym = Symb Char Char
type Gram = Cfg Char Char

readSymb :: Char -> Sym
readSymb c | isUpper c = NT c
           | otherwise = T c

readRHS :: String -> [Symb Char Char]
readRHS [] = []
readRHS (' ':s) = readRHS s -- for convenience
readRHS (c:cs) = readSymb c:readRHS cs

-- (|->) :: nt -> [Symb t nt] -> [Symb t nt]
-- lhs |-> rhs = NT lhs : rhs 
g1 = Cfg [T 'a']
         [NT 'L']
         (NT 'L')
         [("p1", NT 'L' |-> [NT 'L',T 'a'])
         ,("p2", NT 'L' |-> [])
         ]

g3 =  prods2cfg [
  ("t1", NT 'T' |-> rr"T*F"),
  ("t2", NT 'T' |-> rr"F"),
  ("t3", NT 'F' |-> rr"a")
          ] 'T' where rr = readRHS
