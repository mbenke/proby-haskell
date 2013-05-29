data Bit = Zero | One deriving (Eq,Show)

type Cantor = [Bit]
type Pred = Cantor -> Bool

find :: (Cantor->Bool) -> Cantor
find p = if forsome (\a -> p(Zero:a))
     then Zero : find(\a -> p(Zero:a))
     else One  : find(\a -> p(One :a))

forsome, forevery :: (Cantor->Bool) -> Bool
forsome p = p (find p)

forevery p = not(forsome(\a -> not(p a)))

first0 (Zero:xs)=True
first0 _ =False

oneAt 0 (One:xs) = True
oneAt 0 _ = False
oneAt n (_:xs) = oneAt (n-1) xs

all0 (Zero:xs) = all0 xs
all0 _ = False

-- Fail: find alt0
alt0 (Zero:xs) = alt1 xs
alt0 _ = False
alt1 (One :xs) = alt0 xs
alt1 _ = False

(&&&) :: Pred -> Pred -> Pred
p &&& q = \xs -> p xs && q xs