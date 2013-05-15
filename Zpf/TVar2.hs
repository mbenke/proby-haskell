{-# LANGUAGE RankNTypes #-}

import Data.IORef
import Control.Concurrent
import Control.Applicative((<$>),(<*),(*>))

type Lock = MVar ()
newtype STM a = STM {runSTM :: IO a } 

atomically1 :: Lock -> STM a -> IO a
atomically1 l (STM a) = a `locking` l

instance Monad STM where
  return = STM . return
  a >>= k = STM $ runSTM a >>= runSTM . k
     
stmDelay :: Int -> STM ()
stmDelay = STM . threadDelay


newtype TVar a = TVar (IORef a) 
newTVar :: a -> STM (TVar a)
newTVar a =  STM $ TVar <$> newIORef a
  
readTVar :: TVar a -> STM a
readTVar (TVar r) = STM $ readIORef r 
  
  
writeTVar :: TVar a -> a -> STM ()  
writeTVar (TVar r) x = STM $ writeIORef r x

incRef :: TVar Int -> STM ()
incRef var = do 
    val <- readTVar var
    stmDelay 1000         
    writeTVar var (val+1) 

lock :: MVar () -> IO ()
lock = takeMVar

unlock :: MVar () -> IO ()
unlock = flip putMVar ()

locking :: IO a -> MVar () -> IO a
action `locking` l = lock l >> (action <* unlock l)
withLock = flip locking
  
main = do
  gil <- newMVar ()
  let 
    atomically :: STM a -> IO a
    atomically = atomically1 gil
   in main2 atomically

main2 :: (forall a. STM a -> IO a) -> IO ()
main2 atomically = do
  px <- atomically $ newTVar 0
  forkIO $ atomically $ incRef px 
  forkIO $ atomically $ incRef px 
  threadDelay 3000
  atomically (readTVar px) >>= print

