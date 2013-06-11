{-# LANGUAGE Arrows #-}
import Data.List(union) 

-- Swierstra & Duponcheel LL(1) parsers, arrow version

-- static kinfo: nullable, First
data StaticParser s = SP { spNullable :: Bool, spFirst :: [s] } 
newtype DynamicParser s a b = DP {runDP :: (a,[s]) -> (b,[s]) }
data Parser s a b = P (StaticParser s)(DynamicParser s a b)

parse1 :: Parser Char () b -> [Char] -> (b,[Char])
parse1 (P sp dp) [] 
 | spNullable sp = runDP dp ((),[])
 | otherwise = error "unexpected EOT"
parse1 (P sp dp) xs@(x:_)
 | x `elem` spFirst sp = runDP dp ((),xs)
 | otherwise = error $ unwords
    ["unexpected",show x, "expecting one of", showList (spFirst sp) ""]

test1 = parse1 (eps_ 1) ""
eps_ :: b -> Parser s () b
eps_ b = eps (const b)
eps :: (a -> b) -> Parser s a b
eps f = P (SP True []) (DP $ first f)

symbol :: s -> Parser s a s
symbol s = P (SP False [s]) (DP (\(a,x:xs) -> (s,xs)))

class Arrow a where
    arr   :: (b->c) ->  a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b,d) (c,d)

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)
  first f (a, b) = (f a, b)

instance Arrow (DynamicParser s) where
  arr f =  (DP (\(a,xs) -> (f a, xs)))
  (DP p1) >>> (DP p2) = DP $ p1 >>> p2 
  -- first (DP p) = DP $ \((b,d),xs) -> let (c,ys) = p (b,xs) in ((c,d),ys)
  first (DP p) = DP $ rot >>> first p >>> rot where
    rot ((b,s),d) = ((b,d),s)
  -- p :: (b,[s]) -> (c,[s])
  -- first p :: ((b,[s]),d) -> ((c,[s]),d)

instance Eq s => Arrow (Parser s) where
   arr f = P (SP True []) (arr f)
   
   (P (SP nul1 first1) dp1) >>> (P (SP nul2 first2) dp2) =
     P (SP (nul1 && nul2) (union first1 first2)) (dp1 >>> dp2) 
     
   first (P sp dp) = P sp (first dp)
   
class Arrow a => ArrowZero a where
    zeroArrow :: a b c
   
instance Eq s => ArrowZero (Parser s) where
  zeroArrow = P (SP False []) zeroArrow
  
instance ArrowZero (DynamicParser s) where
  zeroArrow = (DP (\(a,xs) -> error "zero"))
  
class ArrowZero a => ArrowPlus a where
    -- | An associative operation with identity 'zeroArrow'.
    (<+>) :: a b c -> a b c -> a b c  
    
instance Eq s => ArrowPlus (Parser s) where
  (P (SP nul1 first1) (DP p1)) <+> (P (SP nul2 first2) (DP p2)) =
    P (SP (nul1 || nul2) (first1++first2)) (DP p) where
    p inp@(a, []) = if nul1 then p1 inp else p2 inp
    p inp@(a,y:ys)= if y `elem` first1 then p1 inp else   
                    if y `elem` first2 then p2 inp else
                    if nul1 then p1 inp else p2 inp
    
