{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where
import Base2  
import Str
import Control.Monad.Writer

vcat :: [Tex] -> Tex
vcat [] = Empty
vcat [t] = t
vcat (t:ts) = Above t (vcat ts)

infixr 6 $$
($$) :: Tex -> Tex -> Tex
($$) = Above

preamble = "\\documentclass{article}"
document :: Tex -> Tex
document body = [str|\begin{document} |] $$ body $$ "\\end{document}"


doc :: Tex
doc = 
  preamble $$
  document "Hello"

td :: Tex
td = execTexW $ do
 "Hello"
 "world"
main = putStrLn . renderTexStr $ doc