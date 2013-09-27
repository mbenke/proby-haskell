{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where
import Hassle.Base
import Hassle.Commands
import Str

preamble = article // 
           usepackage "mbmr" //
           usepackage "mrj"

doc :: Tex
doc = 
  preamble //
  document "Hello"


main = putStrLn . renderTexStr $ doc