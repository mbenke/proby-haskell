{-# LANGUAGE QuasiQuotes #-}
module Main where
import Str
 
longString = [str|This is a multiline string.
It's many lines long.
 
 
It contains embedded newlines. And Unicode:
 
Ἐν ἀρχῇ ἦν ὁ Λόγος
 
It ends here: |]
 
main = putStrLn longString