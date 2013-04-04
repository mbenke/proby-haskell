import Control.Monad
import Control.Monad.Trans
import Data.Monoid
-- Iteratee

data Chunk a
  = Chunk [a]
  | EOF
  deriving (Show, Eq)

instance Monoid (Chunk a) where
    mempty = Chunk []
    mappend (Chunk xs) (Chunk ys) = Chunk $ xs ++ ys

data Step a m b
  = Continue (Chunk a -> Iteratee a m b)
  | Yield b (Chunk a)
  | Error

newtype Iteratee a m b = Iteratee {
  runIteratee :: m (Step a m b)
}

instance Monad m => Monad (Iteratee a m) where
         return = idone
         it >>= k = Iteratee (run it >>= runIteratee . k )

instance MonadTrans (Iteratee el) where
    lift = undefined         
-- getchar

getchar :: Monad m => Iteratee el m (Maybe el)
getchar = icont cont where
        -- cont :: Chunk el -> Iteratee a m b
        cont (Chunk (c:cs)) = iyield (Just c) (Chunk cs)
        cont EOF = idone Nothing -- in Iteratee monad
        cont ch@(Chunk []) = icont cont
        next = undefined

icont :: Monad m => (Chunk a -> Iteratee a m b) -> Iteratee a m b
icont = Iteratee . return . Continue 

ierr :: Monad m => Iteratee a m b
ierr = Iteratee . return $ Error

iyield :: Monad m => b -> Chunk a -> Iteratee a m b
iyield v = Iteratee . return . Yield v

idone :: Monad m => b -> Iteratee a m b
idone = flip iyield EOF
-- run . idone == return
-- runIteratee . idone = return . flip Yield EOF 
-- count_i

count_i :: Monad m => Iteratee el m Int
count_i = icont $ step 0 where
--    step acc (Chunk []) = icont (step acc)
    step acc (Chunk cs) = icont $ step $ acc+length cs
    step acc EOF = iyield acc EOF

-- run

run :: Monad m => Iteratee el m a -> m a
run it = do
    runIteratee it >>= go where 
         -- go :: Step el m a -> m a
         go Error = fail "Error step"
         go (Yield a ch) = return a
         go (Continue k) = run $ k EOF -- runIteratee (k EOF) >>= go

-- Enumerator

type Enumerator el m a =
  Iteratee el m a -> m (Iteratee el m a)


-- enum_list

enum_list :: Monad m => [el] -> Enumerator el m a
enum_list xs = liftM  go . runIteratee where -- runIteratee it >>= return . go where 
         -- go :: Step el m a -> m (Iteratee el m a)
         go (Continue k) = k $ Chunk xs
         -- k :: Chunk el -> Iteratee el m a
         go Error = ierr
         go (Yield a ch) = iyield a ch

-- kompozycja sekwencjalna

(>>>) :: Monad m => Enumerator el m a
  -> Enumerator el m a
  -> Enumerator el m a
(>>>) enor1 enor2 itee = enor2 itee >>= enor1
     

-- type Enumerator el m a = Iteratee el m a -> m (Iteratee el m a)
-- type Enumeratee elo eli m a = Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)

-- enumerator pusty

eof :: Monad m => Enumerator el m a
eof = return


-- Enumeratee

type Enumeratee elo eli m a =
  Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)


-- pipe

infixr 1 .|
(.|) :: Monad m => (Iteratee el m a -> w)
  -> Iteratee el m (Iteratee el' m a)
  -> w
(.|) = (.ijoin)


-- Iteratee el m (Iteratee el' m a) ->
ijoin :: Monad m => Iteratee el m (Iteratee el' m a) -> Iteratee el m a
-- ijoin =  translate . ijoin2 . run
ijoin = lift . join . liftM run . run 

ijoinR :: Monad m => Iteratee el m (Iteratee a m b) -> Iteratee a m b
ijoinR = Iteratee . join . liftM runIteratee . run
-- runIteratee . idone == return . flip Yield EOF 

translate :: Monad m => Iteratee el' m  a -> Iteratee el m a
translate = ijoin2 . liftM idone . run  

ijoin2 :: Monad m => m (Iteratee el m a) -> Iteratee el m a
-- runIteratee iit :: m m (Step el m a)
ijoin2 = Iteratee . join . liftM runIteratee
-- en_filter

en_filter :: Monad m => (el -> Bool) -> Enumeratee el el m a
en_filter = undefined


-- take

take :: Monad m => Int -> Enumeratee el el m a
take = undefined


-- drop

drop :: Monad m => Int -> Enumeratee el el m a
drop = undefined


-- pair

en_pair :: Monad m => Iteratee el m a
  -> Iteratee el m b
  -> Iteratee el m (a,b)
en_pair = undefined
