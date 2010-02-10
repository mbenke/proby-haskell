import SOE

main = runGraphics hello
hello = do 
  w <- openWindow "My First Graphics Program" (300,300)
  drawInWindow w (text (100,200) "Hello Graphics World")
  spaceClose w
     

spaceClose :: Window -> IO ()
spaceClose w
   = do k <- getKey w
        if k==' ' 
           then closeWindow w
           else spaceClose w
