module Main where

import Schizo.Parser

testString  = [ "a b c d",
                "ab cd ef gh" ]
main :: IO ()
main = do
    let str = "12345678abcde"
    let am = oneOrMore digit (Match ([], str))
    putStrLn (show am)
    putStrLn ("Hello World" ++ show (Token ['a', 'b']))