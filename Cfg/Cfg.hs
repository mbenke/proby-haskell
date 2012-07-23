import Data.List(nub,intersperse,intersect)
import Data.Char(isUpper)
-- borrowed from HaGLR
data Symb t nt 
	     = Dollar   -- ^ End of input
             | Root     -- ^ Root symbol
             | T t      -- ^ Terminal symbol
	     | NT nt    -- ^ Non-terminal symbol
             deriving (Eq,Ord,Show,Read)

type RHS t nt = [Symb t nt]
type Prod t nt = (ProdName, [Symb t nt])
type ProdName = String

showSymb Dollar     = "'$'"
showSymb Root      = "'S''"
showSymb (T sy) = show sy    -- todo: filtering for graphviz
showSymb (NT sy) = show sy    -- todo: filtering for graphviz

data Cfg t nt  = Cfg { terminals :: [Symb t nt]             -- ^ Set of Terminal Symbols
                  , nonterminals :: [Symb t nt]             -- ^ Set of Nonterminal Symbols
                  , root :: Symb t nt                       -- ^ Root Symbol
                  , prods :: [Prod t nt]                    -- ^ Set of (named) productions
                  } deriving (Show)

-- | Complete a list of productions to a well-formed grammar.
prods2cfg :: (Eq t, Eq nt) => [Prod t nt] -> nt -> Cfg t nt
prods2cfg l s = Cfg (get_terminals l) (get_nonterminals l) (NT s) l

-- | Compute the list of terminals from a list of productions.
get_terminals :: (Eq t, Eq nt) => [Prod t nt] -> [Symb t nt]
get_terminals l = nub $ filter isterminal $ concat (map snd l)
	where isterminal (T _)  = True
	      isterminal Dollar = True
	      isterminal _      = False

-- | Compute the list of non-terminals from a list of productions.
get_nonterminals :: (Eq t, Eq nt) => [Prod t nt] -> [Symb t nt]
get_nonterminals l = nub $ filter isnonterminal $ concat (map snd l)
	where isnonterminal (NT _)  = True
	      isnonterminal Root = True
	      isnonterminal _      = False

------------------------------------------------------------------------------
-- * Access functions

-- | The left-hand side and th right-hand side of a prodution are easily 
--   defined by the 'head' and 'tail' functions

lhs_prod :: [Symb t nt] -> Symb t nt
lhs_prod (NT nt:_) = NT nt
lhs_prod (Root:_)  = Root 
lhs_prod p = error $ "LHS of the production not a non-terminal" -- ++ show p

rhs_prod :: [Symb t nt] -> RHS t nt
rhs_prod = tail

-- | Checks whether a terminal is  declared as such in a grammar
is_terminal :: (Eq t,Eq nt) => Symb t nt -> Cfg t nt -> Bool 
is_terminal t g = t `elem` (terminals g)


-- | Selects the productions from a 'Cfg' that have a common
--   given non-terminal as left-hand side.
prods_nt :: (Eq t,Eq nt) => Cfg t nt -> Symb t nt -> [[Symb t nt]]
prods_nt g nt = filter ((==) nt .lhs_prod) (map snd (prods g))


-- | Selects the produtions RHSs from a 'Cfg' that have a common
--   given non-terminal as left-hand side.
rhs_nt :: (Eq t,Eq nt) => Cfg t nt -> Symb t nt -> [RHS t nt]
rhs_nt g nt = map rhs_prod (prods_nt g nt)

-- | Selects the produtions RHSs from a 'Cfg' that have a common
--   given non-terminal in the right-hand side.
rhs_with_nt :: (Eq t,Eq nt) => Cfg t nt -> Symb t nt -> [RHS t nt]
rhs_with_nt g nt = filter (elemNT nt) (map (rhs_prod.snd) (prods g))

-- | Selects the productions from a 'Cfg' that have a common
--   given non-terminal as right-hand side.

prods_rhs_with_nt :: (Eq t,Eq nt) => Cfg t nt -> Symb t nt -> [[Symb t nt]]
prods_rhs_with_nt g nt = filter (elemNT nt . rhs_prod) (map snd (prods g))

{-
elemT :: (Eq t, Eq nt) => t -> RHS t nt -> Bool
elemT t rhs = T t `elem` rhs
-}

elemNT :: (Eq t,Eq nt) =>  Symb t nt -> RHS t nt -> Bool
elemNT nt rhs = or (map isNT rhs)
  where 
    isNT nt' = nt' == nt


-- | Verifies whther a sequence of symbols derives in the empty strig or not. 

nullable :: (Eq t,Eq nt) 
         => Cfg t nt      -- ^ Grammar
         -> [Symb t nt]        -- ^ Accumulator: non-terminals visited so far. 
         -> [Symb t nt]        -- ^ List of grammar symbols (from RHS)
         -> Bool
nullable g _ []    = True
nullable g v (T h:t)   = False -- h `is_terminal` g = False -- || h `elem` v = False
nullable g v (nt:t) | nt `elem` v = False
                    | otherwise   = (nullable_nt' g (nt:v) nt) && (nullable g (nt:v) t)

nullable_nt' :: (Eq t,Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> Bool
nullable_nt' g v nt = or $ map (nullable g v) (rhs_nt g nt) 

nullable_nt :: (Eq t,Eq nt) => Cfg t nt -> Symb t nt -> Bool 
nullable_nt g nt = nullable_nt' g [] nt

-- | Computes the set of terminal symbols that begin the strings derived from the given
--   sequence of symbols


first :: (Eq t, Eq nt) 
      => Cfg t nt      -- ^ Grammar
      -> RHS t nt      -- ^ Sequence of grammar symbols
      -> [Symb t nt]           -- ^ 'first' set 
first g sy = maybeAddEot $ first_ g sy where
  maybeAddEot ss | nullable g [] sy = Dollar:ss
                 | otherwise = ss

first_ g sy = go [] sy where
  go _ []                        = []
  go v (h:t) | h `is_terminal` g = [h] 
             | nullable_nt g h   = go (h:v) t  ++ goNT v h
             | otherwise         = goNT v h
  goNT v h   | h `elem` v = [] 
             | otherwise = concatMap (go (h:v)) (rhs_nt g h)

first_nt :: (Eq nt, Eq t) 
        => Cfg t nt                -- ^ Grammar
        -> Symb t nt               -- ^ Nonterminal symbol
        -> [Symb t nt]                     -- ^ 'first' set
--first_nt g nt = nub $ concat $ map (first g) (rhs_nt g nt)
-- MB why not:
first_nt g nt = first g [nt]

-- | Computes the set of terminal symbols that can appear immediately to the 
--   right of a given nonterminal symbol in some sentential form

follow :: (Eq t, Eq nt) 
       => Cfg t nt        -- ^ Grammar
       -> Symb t nt              -- ^ Nonterminal symbol
       -> [Symb t nt]             -- ^ 'follow' set
follow g nt = follow' g [] nt

follow' :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> [Symb t nt]
follow' g v nt | nt `elem` v = [] 
               | otherwise   = all_suffices
  where all_suffices = follow_prods_with_nt g (nt:v) nt (prods_rhs_with_nt g nt)

follow_prods_with_nt :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> [[Symb t nt]] -> [Symb t nt]
follow_prods_with_nt g v nt l = nub $ concat $ map (suffices_after_sy g v nt) l 

suffices_after_sy :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> [Symb t nt] -> [Symb t nt]
suffices_after_sy g v sy p = go rhs
  where 
        go []    = [] 
        go (NT h:t) | sy == (NT h) = f g v lhs t ++ go t
                    | otherwise    = go t
        go (T h:t)  = go t
	go (_:t) = go t
        lhs = lhs_prod p
	rhs = rhs_prod p

f :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> RHS t nt -> [Symb t nt]
f g v lhs rhs | nullable g [] rhs  = first g rhs ++ follow' g v lhs
              | otherwise          = first g rhs

-- end of borrowed
type Sym = Symb Char Char
type Gram = Cfg Char Char

readSymb :: Char -> Sym
readSymb c | isUpper c = NT c
           | otherwise = T c

readRHS :: String -> [Symb Char Char]
readRHS [] = []
readRHS (' ':s) = readRHS s -- for convenience
readRHS (c:cs) = readSymb c:readRHS cs

(|->) :: nt -> [Symb t nt] -> [Symb t nt]
lhs |-> rhs = NT lhs : rhs 
g1 = Cfg [T 'a']
         [NT 'L']
         (NT 'L')
         [("p1", 'L' |-> [NT 'L',T 'a'])
         ,("p2", 'L' |-> [])
         ]

t1 = nullable_nt g1 $ NT 'L'
t2 = first_nt g1 $ NT 'L'
t3 = first g1 [NT 'L']

{-
g2 :: Gram
g2 = prods2cfg [
  'E' |-> rr"E+T",
  'E' |-> rr"T",
  'T' |-> rr"T*F",
  'T' |-> rr"F",
  'F' |-> rr"a" --,
  --'F' |-> rr"(E)"
  ] 'E' where rr = readRHS
-}
g3 =  prods2cfg [
  ("t1", 'T' |-> rr"T*F"),
  ("t2",'T' |-> rr"F"),
  ("t3", 'F' |-> rr"a")
          ] 'T' where rr = readRHS
