module Cfg.Simple.SelectSet where
import Cfg.Simple
import qualified Data.Set as Set
import Data.Set(member,insert)

type SymbolSet = Set.Set Symb
nullable :: Cfg -> Symbols -> SymbolSet -> Bool
nullable g []      visited  = True
nullable g (T h:t) visited  = False 
nullable g (nt:t)  visited 
  | nt `Set.member` visited = False
  | otherwise   = (nullable_nt' g nt v') && (nullable g t v') where
      v' = insert nt visited

nullable_nt' :: Cfg -> Symb -> SymbolSet -> Bool
nullable_nt' g nt v = any (\s -> nullable g s v) (rhs_nt g nt) 

nullable_nt :: Cfg -> Symb -> Bool 
nullable_nt g nt = nullable_nt' g nt Set.empty
