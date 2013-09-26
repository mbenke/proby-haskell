{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where
import Base  
import Str
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
 
main = putStrLn . renderTexStr $ doc