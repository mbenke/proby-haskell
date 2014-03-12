module TreeDict(Dict,empty,insert,insertWith,lookup,fromList,toList) where
import Prelude hiding(lookup)

data Dict k v = Empty | Node k v (Dict k v) (Dict k v) deriving (Eq, Show)

empty :: Dict k v
empty = Empty

insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert = insertWith const

insertWith :: (Ord k) => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f x v Empty = Node x v Empty Empty
insertWith f x v (Node y z l r)
  | x == y = Node x (f v z) l r
  | x <  y = Node y z (new l) r
  | x >  y = Node y z l (new r)
             where new = insertWith f x v

lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup _ Empty = Nothing
lookup x (Node y z l r)
  | x == y = Just z
  | x <  y = lookup x l
  | x >  y = lookup x r

fromList :: Ord k => [(k, v)] -> Dict k v
fromList = foldr (uncurry insert) Empty 

toList :: Dict k v -> [(k, v)]
toList Empty = []
toList (Node x v l r) = toList l ++ [(x,v)] ++ toList r

