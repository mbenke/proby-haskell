import Control.Applicative
import Data.Monoid

newtype State1 s a = State1 {runState1 :: s->(a,s) }
newtype State2 s a = State2 {runState2 :: s->(a,s)}

instance Monad (State1 s) where
    return x = State1 $ \s -> (x,s)
    (State1 f) >>= k = State1 $ \s -> let (a,s') = f s in runState1 (k a) s'

instance Functor (State1 s) where
   fmap f (State1 t) = State1 $ \s -> let (a,s') = t s in (f a,s')
 
instance Functor (State2 s) where
   fmap f (State2 t) = State2 $ \s -> let (a,s') = t s in (f a,s')
 
instance Applicative (State1 s) where
    pure x = State1 $ \s -> (x,s)
    (State1 tf) <*> (State1 tx) = State1 $ \s ->
      let
             (f,s')  = tf s
             (x,s'') = tx s'
      in (f x, s'') 

class Functor f => Monoidal f where
    unit :: f ()
    pair :: f a -> f b -> f (a,b)

instance Monoid s => Monoidal (State2 s) where
    unit = State2 $ \s -> ((),s)
    pair (State2 ta) (State2 tb) = State2 $ \s -> 
        let
         (a,s') = ta s
         (b,s'') = tb s
        in ((a,b),s' <> s'')