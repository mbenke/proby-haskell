import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

{-
> val rf = let val z = ref 0 in (fn x => (z := !z+x; !z)) end;
val rf = fn: int -> int
> rf 3;
val it = 3: int
> rf 3;
val it = 6: int
-}

ref :: IORef Int -> Int -> Int 
ref r x = unsafePerformIO $ modifyIORef r (+x) >> readIORef r

rz :: IORef Int
rz = unsafePerformIO $ newIORef 0

rf :: Int -> Int
rf = ref $ unsafePerformIO $ newIORef 0