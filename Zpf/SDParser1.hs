import Control.Arrow -- for code golf
import Data.Functor((<$>))

-- Swierstra & Duponcheel LL(1) parsers

-- static kinfo: nullable, First
data StaticParser s = SP { spNullable :: Bool, spFirst :: [s] } 
newtype DynamicParser s a = DP([s] -> (a,[s]))
data Parser s a = P (StaticParser s)(DynamicParser s a)

symbol :: s -> Parser s s
symbol s = P (SP False [s]) (DP (\(x:xs) -> (s,xs)))

(<|>) :: Eq s => Parser s a -> Parser s a -> Parser s a
(P (SP nul1 first1) (DP p1)) <|> (P (SP nul2 first2) (DP p2)) =
  P (SP (nul1 || nul2) (first1++first2)) (DP p) where
    p xs = case xs of
      []     -> if nul1 then p1 [] else p2 []
      (y:ys) -> if y `elem` first1 then p1 xs else   
                if y `elem` first2 then p2 xs else
                if nul1 then p1 xs else p2 xs
-- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b ??
-- 

{-
class Arrow a where
    arr   :: (b->c) ->  a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b,d) (c,d)

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)
  first f (a, b) = (f a, b)
-}  
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a->b) -> f a -> f b
  
instance Functor (DynamicParser s) where
  -- fmap f (DP p) = DP $ \xs -> let (a,ys) = p xs in (f a, ys)
  fmap f (DP p) = DP $ first f . p
  
instance Applicative (DynamicParser s) where
  -- pure a = DP (\xs -> (a,xs))
  pure a = DP $ const a &&& id
  (DP pf) <*> (DP pa) = DP $ \xs -> let (f,ys) = pf xs in first f (pa ys)
  
instance Functor (Parser s) where
  fmap f (P sp dp) = P sp (fmap f dp)
  
instance Applicative (Parser s) where
  pure a = P (SP True []) (pure a)
  (P (SP nul1 start1) dpf) <*> (P (SP nul2 start2) dpa) = 
   P (SP (nul1&&nul2) start) dp where 
    start = if nul1 then start1 ++ start2 else start1
    dp = dpf <*> dpa
