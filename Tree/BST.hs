module BST where

data Tree a = Tip | Bin !Size a (Tree a) (Tree a) deriving Show 
type Size = Int

empty :: Tree a
empty = Tip

toList :: Tree a -> [a]
toList t = go t [] where
  go :: Tree a -> [a] -> [a]
  go Tip = id
  go (Bin _ x l r) = go l . (x:) . go r

depth :: Tree a -> Size
depth Tip = 0
depth (Bin s _ _ _) = s

bin :: a -> Tree a -> Tree a -> Tree a
bin x l r = Bin s x l r where s = 1+max (depth l) (depth r) 
leaf x = Bin 1 x Tip Tip
isLeaf (Bin _ _ Tip Tip) = True
isLeaf _ = False
 
balanced :: Tree a -> Bool
balanced Tip = True
balanced (Bin _ _ l r) = balanced l && balanced r && abs (depth l - depth r) < 2

skew :: Tree a -> Size
skew Tip = 0
skew (Bin _ _ l r) = depth l - depth r

-- if skew > +1 rotR
-- if skew < -1 rotL

{- Rotation
Single

    4             2  
   / \           / \ 
   2  5  ->     1   4
  / \              / \
 1   3            3   5

Double

  2             3
 / \          /  \
1   4    ->  2    4
   / \      / \  / \
   3  5   1   Y  U  5
  / \
 Y   U
 
      4            3
     / \         /   \
    2   5 ->    2     4
   / \         / \   / \
  1   3       1   Y U   5
     / \
    Y   U
-}

rot1R, rot1L :: Tree a -> Tree a

rot1R (Bin _ x4 (Bin _ x2 t1 t3) t5) = bin x2 t1 (bin x4 t3 t5)
rot1R t = t
rot2R (Bin _ x4 (Bin _ x2 t1 (Bin _ x3 y u)) t5) = bin x3 (bin x2 t1 y) (bin x4 u t5)
rot2R t = t

-- rotR (Bin _ x4 (Bin _ x2 t1 t3@(Bin _ x3 y u) t5) = bin x3 (bin x2 t1 y) (bin x4 u t5)

rot2L (Bin _ x2 t1 (Bin _ x4 (Bin _ x3 y u) t5)) = bin x3 (bin x2 t1 y) (bin x4 u t5)
rot2L t = t
rot1L (Bin _ x2 t1 (Bin _ x4 t3 t5)) = bin x4 (bin x2 t1 t3) t5
rot1L t = t

-- balancing bin 
-- assumes subtrees are balanced and close in size
bbin :: a -> Tree a -> Tree a -> Tree a
bbin x l@(Bin ls lx ll lr) r@(Bin rs rx rl rr) = bal (bin x l r) where
  bal t | rs > ls+1 = if depth rr > depth rl then rot1L t else rot2L t
        | ls > rs+1 = if depth ll > depth lr then rot1R t else rot2R t
        | otherwise = t
bbin x Tip r@(Bin _ _ rl rr) = (if depth rr > depth rl then rot1L else rot2L) $ bin x Tip r
bbin x l@(Bin _ _ ll lr) Tip = (if depth ll > depth lr then rot1R else rot2R) $ bin x l Tip
bbin x Tip Tip = leaf x

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Tip = bin x Tip Tip
insert x (Bin _ y l r) | x < y = bbin y (insert x l) r
                       | otherwise = bbin y l (insert x r)


fromList :: (Ord a) => [a] -> Tree a
fromList xs = foldr insert Tip xs
