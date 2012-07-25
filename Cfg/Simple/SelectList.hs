module Cfg.Simple.SelectList where
import Cfg.Simple
import Data.List(nub)

-- | does a sequence of symbols derive the empty string?
-- second argument contains symbols already visited
nullable :: Cfg -> Symbols -> Symbols -> Bool
nullable g _ []    = True
nullable g v (T h:t)   = False -- h `is_terminal` g = False -- || h `elem` v = False
nullable g v (nt:t) | nt `elem` v = False
                    | otherwise   = (nullable_nt' g (nt:v) nt) 
                                    && (nullable g (nt:v) t)

nullable_nt' :: Cfg -> Symbols -> Symb -> Bool
nullable_nt' g v nt = any (nullable g v) (rhs_nt g nt) 

nullable_nt :: Cfg -> Symb -> Bool 
nullable_nt g nt = nullable_nt' g [] nt

-- | Computes the set of terminal symbols that begin the strings derived from the given
--   sequence of symbols

first :: Cfg -> RHS -> Symbols
first g sy = maybeAddEot $ first_ g sy where
  maybeAddEot ss | nullable g [] sy = EOT:ss
                 | otherwise = ss
                               
first_ :: Cfg -> RHS -> Symbols
first_ g sy = go [] sy where
  go _ []                        = []
  go v (h:t) | h `is_terminal` g = [h] 
             | nullable_nt g h   = go (h:v) t  ++ goNT v h
             | otherwise         = goNT v h
  goNT v h   | h `elem` v = [] -- go v t
             | otherwise = concatMap (go (h:v)) (rhs_nt g h)

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
               | nt ==  root g = EOT : all_suffices
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

