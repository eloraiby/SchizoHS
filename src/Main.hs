module Main where

import Schizo.Parser

main :: IO ()

main = do
    putStrLn ("Hello World" ++ show (Token ['a', 'b']))