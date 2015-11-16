module Main where

import Schizo.Parser

testString  = [ "a b c d",
                "ab cd ef gh" ]
main :: IO ()
main = do
    exp <- return "{1 a [hello a; hello b; 2 hi there I am 1 2.3 ]}"
    putStrLn (readExpr exp)
