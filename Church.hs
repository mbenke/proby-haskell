module Church where

true, false :: a -> a -> a
true x y = x
false x y = y

type Cbool a = a -> a -> a

cnot :: Cbool(Cbool a) -> Cbool a
cnot x = x false true

and, or :: Cbool(Cbool a) -> Cbool a -> Cbool a
and x y = x y false
or x y = x true y

type Cnum a = (a -> a) -> a -> a
czer s z = z
cone  s z = s z

csuc :: Cnum a -> Cnum a
csuc cn f = f . cn f

ctwo = csuc cone

church :: Int -> Cnum a
church 0 = czer
church n = csuc $ church (n-1)

natural :: Cnum Int -> Int
natural cn = cn (+1) 0

plus1 :: Cnum a -> Cnum a -> Cnum a
plus1 cn dn f = cn f . dn f

plus2 cn = cn csuc

times1 :: Cnum a -> Cnum a -> Cnum a
times1 cn dn = cn . dn

arrow1 cn = cn . times1

arrow2 :: Cnum a
arrow2 cn dn = cn dn
