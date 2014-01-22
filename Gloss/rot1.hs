import Graphics.Gloss
import Graphics.Gloss.Data.Picture

ani :: Float -> Picture
ani t = rotate (12*t) (rectn 80)


rect = Line [(0,0),(0,1),(1,1),(1,0),(0,0)]
rectn n = scale n n rect

main = animate   (InWindow "Nice Window" (300, 300) (10, 10)) white ani

