module Cfg.Simple.SelectSetR where
import Cfg.Simple
import qualified Data.Set as Set
import Data.Set(member,insert,singleton)
import Control.Monad.Reader

type SymbolSet = Set.Set Symb

type VM a = SymbolSet -> a
runVM = ($Set.empty)

isVisited :: Symb -> VM Bool
isVisited s = fmap (member s) ask

ifVisited :: Symb -> VM a -> VM a -> VM a
ifVisited s t e = do
  seen <- isVisited s
  if seen then t else e
  
setVisited :: Symb -> SymbolSet -> SymbolSet
setVisited = insert

withVisited :: Symb -> VM a -> VM a
withVisited s = local $ setVisited s

vand :: VM Bool -> VM Bool -> VM Bool
vand = liftM2 (&&)

vunion = liftM2 Set.union

vunions :: [VM SymbolSet] -> VM SymbolSet
vunions [] = return Set.empty
vunions (m:ms) = m `vunion` vunions  ms

nullable g ss = runVM $ nullable' g ss

nullable' :: Cfg -> Symbols -> VM Bool
nullable' g []      = return True
nullable' g (T h:t) = return False 
nullable' g (nt:t)   =
  ifVisited nt
    (return False) $
    local (setVisited nt) $
               (nullable_nt' g nt) `vand`
               (nullable' g t)
               

nullable_nt' :: Cfg -> Symb -> VM Bool
nullable_nt' g nt v = any (\s -> nullable' g s v) (rhs_nt g nt) 

nullable_nt :: Cfg -> Symb -> Bool 
nullable_nt g nt = runVM $ nullable_nt' g nt

first :: Cfg -> RHS -> SymbolSet
first g sy = maybeAddEot $ first_ g sy where
  maybeAddEot ss | nullable g sy = insert EOT ss
                 | otherwise = ss

first_ :: Cfg -> RHS -> SymbolSet
first_ g sy = runVM $ go sy where
  go []                        = return Set.empty
  go (h:t) | h `is_terminal` g = return $ singleton h 
             | nullable_nt g h   = vunion (goNT h) $ withVisited h (go t)
             | otherwise         = goNT h
  goNT h   = ifVisited h (return Set.empty) $
             fmap Set.unions $ withVisited h $ mapM go (rhs_nt g h)


first_nt :: Cfg -> Symb -> SymbolSet
first_nt g nt = first g [nt]
