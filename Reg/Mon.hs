module Mon where

infixl 5 <>

class Mon m where
  m1 :: m
  (<>) :: m -> m -> m
  
-- Props:
-- left_unit x = m1 <> x == x

instance Mon [a] where
  m1 = []
  (<>) = (++)

