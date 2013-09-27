{-# LANGUAGE OverloadedStrings #-}
module Hassle.Commands where
import Hassle.Base
import Data.String
import Data.List(intersperse)
command :: Tex -> Tex -> Tex
command name arg = command' name [] arg
command' :: Tex -> [Tex] -> Tex -> Tex
command' name opt arg = csname name <> brackets opt<> braces arg
csname name = "\\" <> name
brackets :: [Tex] -> Tex
brackets [] = Empty
brackets ts = "[" <> hcat (intersperse "," ts) <> "]"
braces t = "{" <> t <> "}"
-- brackets 

documentclass c = command "documentclass" c
documentclass' opt c = command' "documentclass" opt c
article :: Tex
article = article' []
article' :: [Tex] -> Tex
article' opt = documentclass' opt "article"
beamer = documentclass "beamer"

usepackage p = command "usepackage" p

environment name body = command "begin" name // body // command "end" name
document body = environment "document" body