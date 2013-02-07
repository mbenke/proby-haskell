module RegExtra where
import Reg
import Mon

data AB = A | B deriving(Eq,Ord,Show)

instance Mon (Reg c) where
  m1 = Eps
  Empty <> y = Empty
  Eps <> y = y
  x <> Eps = x 
  x <> Empty = Empty
  (x :> y) <> z = x:> (y <> z)
  -- x <> (y :> z) = (x <> y) :> z 
  x <> y = x :> y

simpl :: Reg c -> Reg c
simpl (Empty :| x) = simpl x
simpl (x :| Empty) = simpl x
simpl (x :| y) = simpl x :| simpl y
simpl (Empty :> x) = Empty
simpl (x :> Empty) = Empty
simpl (Eps :> x) = x
simpl (x :> Eps) = x
simpl (Many Eps) = Eps
simpl (Many Empty) = Eps
simpl x = x

nullable :: Reg c -> Bool
nullable Eps = True
nullable Empty = False
nullable (x :> y) = nullable x && nullable y
nullable (x :| y) = nullable x || nullable y
nullable (Many _) = True
nullable _ = False

delta :: Reg c -> Reg c
delta r = if nullable r then Eps else Empty

der :: Eq c => c -> Reg c -> Reg c
der c1 (Lit c2) | c1 == c2  = Eps
                | otherwise = Empty
der c (r1 :| r2) = simpl $ der c r1 :| der c r2
der c (r1 :> r2) = simpl $ d1 :| d2  where
                   d1 = simpl $ der c r1 :> r2
                   d2 = simpl $ delta r1 :> der c r2
der c (Many r)   = simpl $ der c r :> Many r
der _ _ = Empty


ders :: Eq c => [c] -> Reg c -> Reg c
ders cs r = foldl (flip der) r cs

accepts,recognizer :: Eq c => Reg c -> [c] -> Bool
recognizer r cs = nullable $ ders cs r
accepts = recognizer


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
