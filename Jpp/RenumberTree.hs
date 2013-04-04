import Control.Monad.State

import Tree

renumber :: Tree a -> Tree Int
renumber t = evalState (renumbers t) 0

renumbers :: Tree a -> State Int (Tree Int)
renumbers Empty = return Empty
renumbers (Node x l r) = do
  l' <- renumbers l
  i <- get
  put $ i+1
  r' <- renumbers r
  return $ Node i l' r'

