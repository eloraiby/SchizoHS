module Main where

import Schizo.Parser

testString  = [ "a b c d",
                "ab cd ef gh" ]
main :: IO ()
main = do
    putStrLn ("Hello World" ++ show (Token ['a', 'b']))