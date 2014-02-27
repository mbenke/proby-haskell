infixr 5 :>
data Complete a = Nil | a :> Complete (a,a) deriving Show

empty :: Complete a
empty = Nil

instance Functor Complete where
  fmap _ Nil = Nil
  fmap f (a :> as) = f a :> fmap (both f) as where
     both f (a,b) = (f a, f b)

left :: Complete a -> Maybe (Complete a)
left Nil = Nothing
left (a :> t) = Just $ fmap fst t
-- bin :: Complete a -> a -> Complete a
-- bin l x r = x :> (l,r) 

t1 = 1 :> Nil
t2 = 1 :> (2,3) :> Nil
t3 = 1 :> (2,3) :> ((4,5),(6,7)) :> Nil