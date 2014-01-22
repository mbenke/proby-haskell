import Graphics.Gloss
import Graphics.Gloss.Data.Picture

main = display (InWindow "Nice Window" (400, 400) (10, 10)) white pict

pict = pictures [
  scale 0.2 0.2 (text "0"),
  translate (-50) (-50) (scale 0.2 0.2 $ text "-50"),
  translate (-100) (-100) (scale 0.2 0.2 $ text "-100")
  ]