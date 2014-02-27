module Tree23 where

data Tip a = Tip
data Node f a 
  = N2 (f a) a (f a)
  | N3 (f a) a (f a) a (f a)

data Depth f a = Zero (f a) | Succ (Depth  (Node f) a)
newtype Tree a = Tree {unTree :: (Depth Tip a) }

empty :: Tree a
empty = Tree (Zero Tip)

empty' :: Depth Tip a
empty' = Zero Tip
 
shift :: Node (Depth f) a -> Depth (Node f) a
shift = undefined

bin1 :: f a -> a -> f a -> Node f a
bin1 t1 a t2 = N2 t1 a t2

bin2 :: Depth f a -> a -> Depth f a -> Depth (Node f) a
bin2 (Zero t1) a (Zero t2) = Zero (bin1 t1 a t2)
bin2 (Succ d1) a (Succ d2) = Succ $ bin2 d1 a d2
bin2 _ _ _ = error "bin2: height mismatch"

bin3 :: Depth f a -> a -> Depth f a -> Depth f a
bin3 l x r = Succ $ bin2 l x r

addEl :: Ord a => a -> Depth f a -> (Depth f a, Maybe (a, Depth f a))
addEl a (Zero t) = (Zero t, Just (a, Zero t)) 

add :: Ord a => a -> Tree a -> Tree a
add a (Tree d) = case addEl a d of
    (d', Nothing) -> Tree d'
    (t1, Just (x,t2)) -> Tree $ Succ undefined