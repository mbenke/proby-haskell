module RegExtra0 where
import Mon
import Reg

instance Mon (Reg c) where
  m1 = undefined
  x <> y = undefined
  
simpl :: Reg c -> Reg c
simpl x = undefined  

nullable :: Reg c -> Bool
nullable x = False

der :: c -> Reg c -> Reg c
der c = undefined