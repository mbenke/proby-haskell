{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Base where
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Monad.Writer
import Data.String
import Str
 
data Tex = Raw Text | Beside Tex Tex | Above Tex Tex | Empty
type TeX = Tex
instance IsString Tex where
  fromString = Raw . fromString

renderTex :: Tex -> Text
renderTex Empty = T.empty
renderTex (Raw t) = t
renderTex (Beside t1 t2) = T.append (renderTex t1) (renderTex t2)
renderTex (Above t1 t2) = T.append (renderTex t1) $ T.cons '\n' (renderTex t2)

renderTexStr = T.unpack . renderTex

instance Monoid Tex where
  mempty = Empty
  mappend = Beside

type TexW = Writer Tex
execTexW :: TexW () -> Tex
execTexW = execWriter

line :: TeX -> TexW ()
line t = tell $ t <> "\n"
{- might try to make strings writers, but this seems not  to work...
instance IsString (Writer Tex ()) where
         fromString = tell . fromString
-}