{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.String
import Str
 
data Tex = Raw Text | Beside Tex Tex | Above Tex Tex | Empty
instance IsString Tex where
  fromString = Raw . fromString
  
renderTex :: Tex -> Text
renderTex Empty = T.empty
renderTex (Raw t) = t
renderTex (Beside t1 t2) = T.append (renderTex t1) (renderTex t2)
renderTex (Above t1 t2) = T.append (renderTex t1) $ T.cons '\n' (renderTex t2)

instance Monoid Tex where
  mempty = Empty
  mappend = Beside
  
doc :: Tex
doc = [str|
\begin{document}
Hello
\end{document} |]
 
main = putStrLn . T.unpack . renderTex $ doc