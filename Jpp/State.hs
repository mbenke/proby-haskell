module State where
data State s a = State {runState :: s -> (a,s)}

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State t) >>= k = State $ \s0 -> let (x,s1) = t s0 in runState (k x) s1

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((),s)

evalState stateMonad value = fst ( runState stateMonad value )
execState stateMonad value = snd ( runState stateMonad value )

