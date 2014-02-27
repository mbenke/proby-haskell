module UTree where
import Prelude hiding(lookup)

data Tree a 
  = Tip 
  | Bin (Tree a) a (Tree a)
  | Tri (Tree a) a (Tree a) a (Tree a)
  deriving Show

empty :: Tree a 
empty = Tip

mkLeaf :: a -> Tree a
mkLeaf a = Bin Tip a Tip

mkLeaf3 :: Ord a => a -> a -> Tree a
mkLeaf3 a b = Tri Tip (min a b) Tip (max a b) Tip

isLeaf :: Tree a -> Bool
isLeaf (Bin Tip _ Tip) = True
isLeaf (Tri Tip _ Tip _ Tip) = True
isLeaf _ = False

-- lookupBy :: (a -> Bool) -> Tree a -> Maybe a
-- lookupBy p 

lookup :: Ord a => a -> Tree a -> Maybe a
lookup _ Tip = Nothing
lookup x (Bin l m r) 
  | x == m = Just x
  | x < m = lookup x l
  | x > m = lookup x r

data Carry a = NoCarry | Carry a (Tree a) -- assume Carry a t ensures t <= a

insert x t = case addEl x t of
  (t, NoCarry) -> t
  (t, Carry a t1) -> Bin t1 a t

fromList :: Ord a => [a] -> Tree a
fromList xs = foldr insert empty xs

addEl :: Ord a => a -> Tree a -> (Tree a, Carry a)
addEl a Tip = (Tip, Carry a Tip)
addEl a (Bin Tip m Tip) = (mkLeaf3 a m, NoCarry)
addEl a (Bin t1 m t2) 
 | a < m = (t',NoCarry) where 
  t' = case addEl a t1 of
     (t1', NoCarry) ->  Bin t1' m t2  -- t1' < m
     (t1', Carry y t0) -> -- t0 < y < t1'
        Tri t0 y t1' m t2
 | a >= m = (t',NoCarry) where 
  t' = case addEl a t2 of
     (t2', NoCarry) -> Bin t1 m t2' 
     (t2', Carry y t0) -> Tri t1 m t2' y t2
addEl a (Tri t1 x t2 y t3) 
 | a < x = case addEl a t1 of 
     (t1', NoCarry) -> (Tri t1' x t2 y t3, NoCarry)
     (t1', Carry z t0) -> -- t0 < z < x