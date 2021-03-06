import Data.IORef
import Control.Concurrent
import Control.Applicative((<$>),(<*),(*>))

newtype STM a = STM { runSTM ::(IO a)}
data TVar a = TVar (MVar ()) (IORef a) 
newTVar :: a -> STM (TVar a)
newTVar a = STM $ do
  l <- newMVar ()
  r <- newIORef a
  return $ TVar l r
  
readTVar :: TVar a -> STM a
readTVar (TVar l r) = STM $ readIORef r `locking` l
  
  
writeTVar :: TVar a -> a -> STM ()  
writeTVar = undefined

incRef :: IORef Int -> IO ()
incRef var = do { val <- readIORef var
                ; threadDelay 1000         
       	        ; writeIORef var (val+1) }

lock :: MVar () -> IO ()
lock = takeMVar

unlock :: MVar () -> IO ()
unlock = flip putMVar ()

locking :: IO a -> MVar () -> IO a
action `locking` l = lock l >> (action <* unlock l)
  
main = do
  gil <- newMVar ()
  let atomically a = a `locking` gil
  main2 atomically
          
main2 atomically = do
  px <- newIORef 0
  forkIO $ atomically $ incRef px 
  forkIO $ atomically $ incRef px 
  threadDelay 3000
  readIORef px >>= print
