module RegExtra where
import Reg
import Mon
import Data.List(nub)

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   r1 === r2 = norm r1 == norm r2

instance Mon (Reg c) where
  m1 = Eps
  -- Empty <> y = Empty
  -- x <> Empty = Empty
  Eps <> y = norm y
  x <> Eps = norm x
  x <> y = mkSeq $(splitSeq x) ++ (splitSeq y)
  
mkSeq :: [Reg c] -> Reg c
mkSeq [] = Eps
mkSeq xs = foldr1 (:>) xs

splitSeq :: Reg c -> [Reg c]
splitSeq (x :> y) = splitSeq x ++ splitSeq y
splitSeq Eps = []
splitSeq x = [x]

normSeq = mkSeq . splitSeq
norm = normSeq

step :: Eq c => Reg c -> Reg c
step r@(x :| y) = unEmptyOr r
step r@(x :> y) = step x <> step y
step (Many (Many x)) = Many x
step (Many Eps) = Eps
step (Many r) = Many $ unEpsOr $ step r
step x = x

runSteps :: Eq c => Int -> Reg  c -> Reg c
runSteps 0 x = x
runSteps n x = if x == x' then x else runSteps (n-1) x' where x' = step x

simpl, simply :: Eq c => Reg c -> Reg c
simpl x = runSteps 5 x
simply = simpl

{-
simpl :: Eq c => Reg c -> Reg c
simpl (Empty :| x) = simpl x
simpl (Eps :| Many x) = Many (simpl x)
simpl (x :| Empty) = simpl x
simpl (x :| y) | x' == y' = x' where  [x',y'] = map simpl [x,y]
simpl ((x:| y) :| z) = simpl (x :| (y :| z))
simpl (x :| y) = unEmptyOr (simpl x :| simpl y)
simpl (Empty :> x) = Empty
simpl (x :> Empty) = Empty
simpl (Eps :> x) = x
simpl (x :> Eps) = x
simpl ((x :> y) :> z) = simpl x <> simpl y <> simpl z
simpl (x :> y) = simpl x <> simpl y
simpl (Many Eps) = Eps
simpl (Many Empty) = Eps
-- simpl (Many (Eps :| x)) = simpl (Many (unEpsOr x))
simpl (Many x@(_:|_)) = case unEpsOr y of
                      Empty -> Eps
                      _ -> Many (unEpsOr y)
                 where y = simpl x
simpl (Many (Many x)) = Many (simpl x)
simpl (Many x) = Many (simpl x)
-}
-- simpl x = x
-- simply x = runSimpl 3 x
  
runSimpl 0 x = x
runSimpl n x = if x == x' then x else runSimpl (n-1) x' where x' = simpl x

splitOr :: Reg c -> [Reg c]
splitOr (x :| y) = splitOr x ++ splitOr y
splitOr Empty = []
splitOr x = [x]

mkOr [] = Empty
mkOr [x] = x
mkOr (x:xs) = x :| mkOr xs

isEps Eps = True
isEps _ = False

unEpsOr  = mkOr . filter (not . isEps) . splitOr
unEmptyOr :: Eq c => Reg c -> Reg c
unEmptyOr = mkOr . nub . filter (not . empty) . splitOr

nullable :: Reg c -> Bool
nullable Eps = True
nullable Empty = False
nullable (x :> y) = nullable x && nullable y
nullable (x :| y) = nullable x || nullable y
nullable (Many _) = True
nullable _ = False

ifNullable r t e = if nullable r then t else e

delta :: Reg c -> Reg c
delta r = if nullable r then Eps else Empty

empty :: Reg c -> Bool
empty Empty = True
empty (l :> r) = empty l || empty r
empty (l :| r) = empty l && empty r
empty _ = False

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c = not . empty . der c

der :: Eq c => c -> Reg c -> Reg c
der c1 (Lit c2) | c1 == c2  = Eps
                | otherwise = Empty
der c (r1 :| r2) = simpl $ der c r1 :| der c r2
der c (r1 :> r2) = simpl $ d1 :| d2  where
                   d1 = simpl $ der c r1 <> r2
                   d2 = simpl $ delta r1 <> der c r2
der c (Many r)   = simpl $ der c r <> Many r
der _ _ = Empty


ders :: Eq c => [c] -> Reg c -> Reg c
ders cs r = foldl red r cs where
  red r c = simpl (der c r)

accepts,recognizer :: Eq c => Reg c -> [c] -> Bool
recognizer r cs = nullable $ ders cs (simply r)
accepts = recognizer

match :: Eq a => Reg a -> [a] -> Maybe [a]
match r [] = if nullable r then Just [] else Nothing
match r (c:cs) 
  | empty r' = match r []
  | otherwise = match r' cs >>= return . (c:)
  where r' = der c r
        
match1 :: Eq a => Reg a -> [a] -> Maybe ([a],[a])
match1 r [] = if nullable r then Just ([],[]) else Nothing
match1 r s@(c:cs) 
  | empty r' = match1 r [] >> return ([],s)
  | otherwise = do { (m,rest) <- match1 r' cs ; return (c:m,rest) }
  where r' = der c r

search :: Eq a => Reg a -> [a] -> Maybe ([a],[a])
search r [] = if nullable r then return ([],[]) else Nothing
search r s@(c:cs) 
  | empty r' = if nullable r then return ([],s) else search r cs
  | otherwise = do { (m,rest) <- search r' cs ; return (c:m,rest) }
  where r' = der c r

findall ::  Eq a => Reg a -> [a] -> [[a]]
findall r cs = maybe [] f (search r cs) where
  f (m,t) = m:findall r t
{-
findall r cs = case search r cs of
  Nothing -> []
  Just (m,t1) -> m:findall r t1 
-}
char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r


-- problematic
prx = (((Lit B :| Lit A) :| Many Eps) :| (Many (Lit B) :> (Many Empty :> (Eps :> Eps)))) :> (Many (Lit A :| Lit B) :> (Many (Lit A) :| (Lit A :> Lit A)))
pry = Many (((Lit A :| Lit B) :| (Lit A :| Eps)) :> ((Eps :| Eps) :| Many (Lit B)))
pcs1 = ders [B,A,A,A] (pry <> prx)
pyx = pry <> prx
pyx1 = Many ((Lit A :| (Lit B :| (Lit A :| Eps))) :> Many (Lit B)) :> ((Lit B :| (Lit A :| Many (Lit B))) :> (Many (Lit A :| Lit B) :> (Many (Lit A) :| (Lit A :> Lit A))))
pcs = [B,A,A,A,A,B,B,A,B,B,B,A,A,B,B,A,B]
pref k = ders (take k pcs) (simply pyx)
