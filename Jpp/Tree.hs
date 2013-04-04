module Tree where


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

instance Functor Tree where
	 fmap f Empty = Empty
	 fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

toList :: Tree a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ [x] ++ toList r

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
  | x < y = Node y (insert x l) r
  | True = Node y l (insert x r) 

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = insert x $ fromList xs
