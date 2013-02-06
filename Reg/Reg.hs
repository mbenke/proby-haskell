module Reg where

data Reg c
  = Lit c
  | Reg c :> Reg c
  | Reg c :| Reg c
  | Many (Reg c)
  | Eps
  | Empty
  deriving (Eq,Show)
