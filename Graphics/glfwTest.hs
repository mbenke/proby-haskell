import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW

main = do
  initialize 
  openWindow (GL.Size 400 400) [DisplayAlphaBits 8] Window
  waitESC
  closeWindow
  terminate

waitESC = do
	waitEvents
	p <- getKey ESC
	if (p == Press) then return () else waitESC

