{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where
import Hassle.Base
import Hassle.Commands
import Str

preamble = article // 
           [str|
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}|]


doc :: Tex
doc = 
  preamble //
  document "Hello"


main = putStrLn . renderTexStr $ doc