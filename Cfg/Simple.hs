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


-- | Verifies whther a sequence of symbols derives in the empty strig or not. 
-- second argument contains symbols already visited
nullable :: Cfg -> Symbols -> Symbols -> Bool
nullable g _ []    = True
nullable g v (T h:t)   = False -- h `is_terminal` g = False -- || h `elem` v = False
nullable g v (nt:t) | nt `elem` v = False
                    | otherwise   = (nullable_nt' g (nt:v) nt) && (nullable g (nt:v) t)

nullable_nt' :: Cfg -> Symbols -> Symb -> Bool
nullable_nt' g v nt = or $ map (nullable g v) (rhs_nt g nt) 

nullable_nt :: Cfg -> Symb -> Bool 
nullable_nt g nt = nullable_nt' g [] nt

-- | Computes the set of terminal symbols that begin the strings derived from the given
--   sequence of symbols

first :: Cfg -> RHS -> Symbols
first g sy = go [] sy where
  go _ []                        = []
  go v (h:t) | h `is_terminal` g = [h] 
             | h `elem` v        = go v t
             | nullable_nt g h   = go (h:v) t  ++ goNT v h
             | otherwise         = goNT v h
  goNT v h = concatMap (go (h:v)) (rhs_nt g h)

first_nt :: Cfg -> Symb -> Symbols
first_nt g nt = first g [nt]

-- | Computes the set of terminal symbols that can appear immediately to the 
--   right of a given nonterminal symbol in some sentential form

follow :: Cfg         -- ^ Grammar
       -> Symb        -- ^ Nonterminal symbol
       -> Symbols     -- ^ 'follow' set
follow g nt = follow' g [] nt

-- follow' :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> [Symb t nt]
follow' g v nt | nt `elem` v = [] 
               | otherwise   = all_suffices
  where all_suffices = follow_prods_with_nt g (nt:v) nt (prods_rhs_with_nt g nt)

-- follow_prods_with_nt :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> [[Symb t nt]] -> [Symb t nt]
follow_prods_with_nt g v nt l = nub $ concat $ map (suffices_after_sy g v nt) l 

suffices_after_sy :: Cfg -> [Symb] -> Symb -> Prod -> [Symb]
suffices_after_sy g v sy p = go rhs
  where 
        go []    = [] 
        go (NT h:t) | sy == (NT h) = f g v (NT lhs) t ++ go t
                    | otherwise    = go t
        go (T h:t)  = go t
	go (_:t) = go t
        lhs = prod_lhs p
	rhs = prod_rhs p

f ::Cfg  -> [Symb] -> Symb -> RHS -> [Symb]
f g v lhs rhs | nullable g [] rhs  = first g rhs ++ follow' g v lhs
              | otherwise          = first g rhs



readSymb :: Char -> Sym
readSymb c | isUpper c = NT c
           | otherwise = T c

readRHS :: String -> RHS
readRHS [] = []
readRHS (' ':s) = readRHS s -- for convenience
readRHS (c:cs) = readSymb c:readRHS cs

(|->) :: Char -> RHS -> Prod
lhs |-> rhs = Prod lhs  rhs 


g1 = prods2cfg
         [ 'L' |-> [NT 'L',T 'a']
         , 'L' |-> []         
         ] 'L'

t1 = nullable_nt g1 $ NT 'L'
t2 = first_nt g1 $ NT 'L'
t3 = first g1 [NT 'L']

g2 :: Gram
g2 = prods2cfg [
  'E' |-> rr"E+T",
  'E' |-> rr"T",
  'T' |-> rr"T*F",
  'T' |-> rr"F",
  'F' |-> rr"a" --,
  --'F' |-> rr"(E)"
  ] 'E' where rr = readRHS

g3 =  prods2cfg [
  'T' |-> rr"T*F",
  'T' |-> rr"F",
  'F' |-> rr"a"
          ] 'F' where rr = readRHS
