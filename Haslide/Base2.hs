{-# LANGUAGE QuasiQuotes, OverloadedStrings, FlexibleInstances #-}
module Base2 where
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

newtype TexW a = TexW {unwrapTexW :: Writer Tex a}
instance Monad TexW where
   return = TexW . return
   (TexW m) >>= k = TexW $ m >>= (unwrapTexW . k)
   fail s = return undefined

-- instance MonadWriter Tex TexW where
textell :: Tex -> TexW ()
textell = TexW . tell

execTexW :: TexW () -> Tex
execTexW = execWriter . unwrapTexW

line :: TeX -> TexW ()
line t = textell $ t <> "\n"

instance IsString (TexW a) where
         fromString s = (textell (fromString s)) >> fail "cast"
