{-# LANGUAGE FlexibleInstances #-}
module Cfg.Simple where
import Data.List(nub,intersperse,intersect)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char(isUpper)

type Sym = Symb
type Gram = Cfg

data Symb 
	     = EOT     -- ^ End of input
             | Root    -- ^ Root symbol
             | T Char  -- ^ Terminal symbol
	     | NT Char -- ^ Non-terminal symbol
             deriving (Eq,Ord,Show,Read)

type Symbols = [Symb]
type RHS = Symbols
data Prod = Prod {prod_lhs::Char, prod_rhs::RHS} deriving Show

showSymb EOT     = "#"
showSymb Root      = "Z"
showSymb (T sy) =  [sy]
showSymb (NT sy) = [sy]

class HasSymbols t where
  getTerminals :: t -> Symbols
  getNonterminals :: t -> Symbols

instance HasSymbols Symb where
  getTerminals s@(T _) = [s]
  getTerminals _ = []
  getNonterminals s@(NT _) = [s]
  getNonterminals s = []
  
instance HasSymbols t => HasSymbols [t] where
  getTerminals = nub . concatMap getTerminals
  getNonterminals = nub . concatMap getNonterminals

instance HasSymbols Prod where
  getTerminals = nub . getTerminals . prod_rhs
  getNonterminals (Prod l r) = nub $ NT l:getNonterminals r

data Cfg = Cfg {    terminals :: Symbols         -- ^ Set of Terminal Symbols
                  , nonterminals :: Symbols      -- ^ Set of Nonterminal Symbols
                  , root :: Symb                 -- ^ Root Symbol
                  , prods :: Map.Map Char [Prod] -- ^ Set of productions
                  } deriving (Show)

instance HasSymbols Cfg where
  getTerminals = terminals
  getNonterminals = nonterminals
  
prod2Pair :: Prod -> (Char,Prod)
prod2Pair p = (prod_lhs p, p)

-- | Complete a list of productions to a well-formed grammar.
prods2cfg :: [Prod] -> Char -> Cfg 
prods2cfg l s = Cfg (getTerminals l) (getNonterminals l) (NT s) $ 
                foldr addProd Map.empty l where
                  addProd p = Map.insertWith (++) (prod_lhs p) [p]

-- | Checks whether a terminal is  declared as such in a grammar
is_terminal :: Symb -> Cfg -> Bool 
is_terminal t g = t `elem` (getTerminals g)

prods_nt :: Cfg -> Symb -> [Prod]
prods_nt g (NT c) = Map.findWithDefault [] c $ prods g

rhs_nt :: Cfg -> Symb -> [RHS]
rhs_nt g  = map prod_rhs . prods_nt g

prodsList :: Cfg -> [Prod]
prodsList g = concatMap snd . Map.toList $ prods g

-- | Selects the produtions RHSs from a 'Cfg' that have a common
--   given non-terminal in the right-hand side.
rhs_with_nt :: Cfg -> Symb -> [RHS]
rhs_with_nt g nt = filter (elemNT nt) (map prod_rhs $prodsList g)

-- | Selects the productions from a 'Cfg' that have a common
--   given non-terminal as right-hand side.

prods_rhs_with_nt :: Cfg -> Symb -> [Prod]
prods_rhs_with_nt g nt = filter (elemNT nt . prod_rhs) (prodsList g)

{-
elemT :: (Eq t, Eq nt) => t -> RHS t nt -> Bool
elemT t rhs = T t `elem` rhs
-}

elemNT :: Symb -> RHS -> Bool
elemNT nt rhs = or (map isNT rhs)
  where 
    isNT nt' = nt' == nt

readSymb :: Char -> Sym
readSymb c | isUpper c = NT c
           | otherwise = T c

readRHS :: String -> RHS
readRHS [] = []
readRHS (' ':s) = readRHS s -- for convenience
readRHS (c:cs) = readSymb c:readRHS cs
rr = readRHS

(|->) :: Char -> RHS -> Prod
lhs |-> rhs = Prod lhs  rhs 


g1 = prods2cfg
         [ 'L' |-> [NT 'L',T 'a']
         , 'L' |-> []         
         ] 'L'
         
g2 :: Gram
g2 = prods2cfg [
  'E' |-> rr"E+T",
  'E' |-> rr"T",
  'T' |-> rr"T*F",
  'T' |-> rr"F",
  'F' |-> rr"a",
  'F' |-> rr"(E)"
  ] 'E' where rr = readRHS

g3 =  prods2cfg [
  'T' |-> rr"T*F",
  'T' |-> rr"F",
  'F' |-> rr"a"
          ] 'F' where rr = readRHS
