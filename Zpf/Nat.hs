{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

one   = undefined :: One
two   = undefined :: Two
three = undefined :: Three
four  = undefined :: Four

class Add a b c | a b -> c where
  add :: a -> b -> c
  add = undefined
instance              Add  Zero    b  b
instance Add a b c => Add (Succ a) b (Succ c)
