-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Universidade do Minho, 2004
-- License     :  LGPL
-- 
-- Maintainer  :  João Saraiva - jas@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Representing Context-free Grammars in Haskell
--
-----------------------------------------------------------------------------

module Language.ContextFree.Cfg where

------------------------------------------------------------------------------

import Data.List(nub,intersperse,intersect)
import Debug.Trace
------------------------------------------------------------------------------
-- * Representation

-- | The type of Context Free Grammars.
--   A production is modeled as a list of symbols, where the head of the
--   list models the LHS and the tail models the RHS.

data Cfg t nt  = Cfg { terminals :: [Symb t nt]             -- ^ Set of Terminal Symbols
                  , nonterminals :: [Symb t nt]             -- ^ Set of Nonterminal Symbols
                  , root :: Symb t nt                       -- ^ Root Symbol
                  , prods :: [Prod t nt]                    -- ^ Set of (named) productions
                  } deriving (Show)

cfg2dotGraph :: (Eq t, Eq nt, Show t, Show nt) => Cfg t nt -> String	
cfg2dotGraph gr
  = let pr     = map ((\x -> (head x, tail x)).snd) (prods gr)
        header = "digraph G {" ++ "\n" ++ "rankdir=LR;\n"
        footer = "}"
        aux (h,t) 
          = let edge  = nodeName1 ++ " -> " ++ nodeName2 ++ ";\n"
                color = if (h == root gr) 
                         then ",fontcolor=green" 
                         else "" 
                node1 = nodeName1 ++ " [label =" ++ nodeName1 ++ ",shape=box" ++ color ++"];\n"
                node2 = nodeName2 ++ " [label =" ++ nodeName2 ++ ",shape=box,fontcolor=white,color=black,style=filled];\n"
                nodeName1 = quote $ sy2NodeName h
                nodeName2 = quote $ rhs2NodeName t
			    
                edges = concatMap rhs_nt t
                rhs_nt (T _) = ""
                rhs_nt s     = nodeName2 ++ " -> " ++ quote (sy2NodeName s)
            in edge ++ node1 ++ node2 ++ edges
    in header ++ (concat.reverse) (map aux pr) ++ footer  

sy2NodeName (T sy) = show sy
sy2NodeName sy     = (tail.init.showSymb) sy
rhs2NodeName rhs   = "::=  "++ (concat (intersperse ("  ") (map sy2NodeName rhs)))++" "
			    


quote s = "\""++s++"\""	

cfg2dotList :: (Eq t, Eq nt, Show t, Show nt) => Cfg t nt -> String	
cfg2dotList gr
  = let pr    = map ((\x -> (head x, tail x)).snd) (prods gr)
        named = zip pr ["node" ++ show i | i <- [1..]]
        header = "digraph G {" ++ "\n" ++ "rankdir=LR;\n"
        footer = "}"
        aux ((h,t),n) = let edge  = n ++ "h -> " ++ n ++ "t;\n"
                            color = if (h == root gr) 
                                     then ",fontcolor=green" 
                                     else "" 
                            node1 = n ++ "h [label =\"" ++ (tail.init.showSymb) h ++ "\",shape=plaintext" ++ color ++"];\n"
                            node2 = n ++ "t [label =\"" ++ concat (intersperse ("   ") (map faux t)) ++"\",shape=plaintext];\n"
                            faux (T sy) = show sy
                            faux sy     = (tail.init.showSymb) sy
                        in edge ++ node1 ++ node2
	in header ++ (concat.reverse) (map aux named) ++ footer  
	      

type RHS t nt = [Symb t nt]
type Prod t nt = (ProdName, [Symb t nt])
type ProdName = String


data Symb t nt 
	     = Dollar   -- ^ End of input
             | Root     -- ^ Root symbol
             | T t      -- ^ Terminal symbol
	     | NT nt    -- ^ Non-terminal symbol
             deriving (Eq,Ord,Show,Read)

{-
instance (Show sy) => Show (Symb sy) where
  show = showSymb
-}
showSymb Dollar     = "'$'"
showSymb Root      = "'S''"
showSymb (T sy) = show sy    -- todo: filtering for graphviz
showSymb (NT sy) = show sy    -- todo: filtering for graphviz


-- | To make the notation of the grammars as similar to BNF as possible 
--   we define an infix operator to denote the usual 'derives to' operator

(|->) :: a -> [a] -> [a]
l |-> r = l : r


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

------------------------------------------------------------------------------
-- * Simple queries on grammars

-- | Computes the length of a prodution: the number of symbols on its right-hand side

sizeProd :: [Symb t nt] -> Int 
sizeProd p = length p - 1


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

------------------------------------------------------------------------------
-- * Advanced queries on grammars

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
first g sy = first' g [] sy


-- | Computes the set of terminal symbols that begin the strings derived from the given
--   nonterminal symbol

first_N :: (Eq nt, Eq t) 
        => Cfg t nt                -- ^ Grammar
        -> Symb t nt               -- ^ Nonterminal symbol
        -> [Symb t nt]                     -- ^ 'first' set
first_N g nt = nub $ concat $ map (first g) (rhs_nt g nt)



first' :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> RHS t nt -> [Symb t nt]
first' g _ []                        = []
first' g v (h:t) | h `is_terminal` g = [h] 
                 | h `elem` v        = first' g v t
                 | nullable_nt g h   = (first' g (h:v) t) ++ (concat (map (first' g (h:v)) (rhs_nt g h)))
                 | otherwise         = concat $  map (first' g (h:v)) (rhs_nt g h)



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
suffices_after_sy g v sy p = suffices_after_sy' g sy rhs
  where 
        lhs = lhs_prod p
	rhs = rhs_prod p
        suffices_after_sy' g sy []    = [] 
        suffices_after_sy' g sy (NT h:t) | sy == (NT h)   = f g v lhs t ++ (suffices_after_sy' g sy t)
                                         | otherwise = suffices_after_sy' g sy t
        suffices_after_sy' g sy (T h:t)  = suffices_after_sy' g sy t
	suffices_after_sy' g sy (_:t) = suffices_after_sy' g sy t

f :: (Eq t, Eq nt) => Cfg t nt -> [Symb t nt] -> Symb t nt -> RHS t nt -> [Symb t nt]
f g v lhs rhs | nullable g [] rhs  = first g rhs ++ follow' g v lhs
              | otherwise          = first g rhs


-- | Computes the set of terminal symbols that begins the strings derived 
--   from the given production

lookahead :: (Eq t, Eq nt) 
          => Cfg t nt        -- ^ Grammar
          -> [Symb t nt]       -- ^ Production
          -> [Symb t nt]             -- ^ 'lookahead' set
lookahead g p | nullable g [] (rhs_prod p) = nub $ first g (rhs_prod p) ++ follow g  (lhs_prod p)
              | otherwise                  = nub $ first g (rhs_prod p) 


all_lookaheads :: (Eq t, Eq nt)
               => Cfg t nt    -- ^ Grammar
               -> [[Symb t nt]]       -- ^ Sequence of 'lookahead' set
all_lookaheads g = map (lookahead g) (map snd (prods g))


-- | Computes the lookahead set of a nonterminal symbols, that is the union of 
--   the lookaheads of the productions with this nonterminal symbol as 'lhs' 

lookaheads_nt :: (Eq t, Eq nt)
              => Cfg t nt      -- ^ Grammar
              -> nt          -- ^ Nonterminal symbol
              -> [[Symb t nt]]      -- ^ Sequence of 'lookahead' set
lookaheads_nt g nt = map (lookahead g) (prods_nt g (NT nt))

------------------------------------------------------------------------------
-- * Check the category in which the grammar falls.

-- | Verifies whether the given non-terminal verifies the LL(1) condition or not.

ll_1_nt :: (Eq nt,Eq t)
        => Cfg t nt            -- ^ Grammar
        -> Symb t nt           -- ^ Nonterminal symbol
        -> Bool
ll_1_nt g (NT nt) = and (map (== []) (intersects xs))
  where xs = lookaheads_nt g nt

intersects []    = []
intersects (h:t) = (map (intersect h) t) ++ (intersects t)


-- ll_1 g = map (lookaheads_nt g) (nonterminals g)

-- | Verifies whether a grammar verifies the LL(1) condition or not.

ll_1 :: (Eq t, Eq nt) => Cfg t nt -> Bool
ll_1 g = and $ map (ll_1_nt g) (nonterminals g)


------------------------------------------------------------------------------



ex = Cfg [T 'a',T 'b',T 'c']
         [NT 'A',NT 'B',NT 'C']
         (NT 'A')
         [ ("p0",NT 'A' |-> [NT 'B', NT 'A'])
         , ("p1",NT 'A' |-> [NT 'D'])
         , ("p2",NT 'B' |-> [NT 'A'])
         , ("p3",NT 'B' |-> [T 'c'])
         , ("p4",NT 'C' |-> [T 'c'])
         , ("p5",NT 'C' |-> [NT 'D'])
         , ("p6",NT 'D' |-> [NT 'C'])
         ]

ex2 = Cfg [T 'a',T 'b',T 'c']
          [NT 'A',NT 'B',NT 'C',NT 'D']
          (NT 'A')
          [ ("p0",NT 'A' |-> [NT 'A'])
          , ("p1",NT 'A' |-> [NT 'B', NT 'D'])        
          , ("p2",NT 'A' |-> [T 'c'])
          , ("p3",NT 'B' |-> [NT 'A'])
          , ("p4",NT 'B' |-> [NT 'C'])
          , ("p5",NT 'C' |-> [NT 'D'])
          , ("p6",NT 'C' |-> [])
          , ("p7",NT 'D' |-> [NT 'C'])
          , ("p8",NT 'D' |-> [NT 'C', T 'a', NT 'C'])
          , ("p9",NT 'D' |-> [T 'b'])
          ]


x = nullable_nt ex2 (NT 'A')

