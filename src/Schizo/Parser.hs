module Schizo.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.Parsec.Prim as ParsecPrim
import Text.Parsec.Combinator as ParsecComb

import System.Environment
import Control.Monad

data SchExp  = Symbol       String
             | Operator     String
             | List         [SchExp]
             | Tuple        [SchExp]
             | Sequence     [SchExp]
             | Application  (SchExp, [SchExp])
             | Int64        Integer
             | Float64      Double
             | String       String
             | Bool         Bool
             deriving Show

spaces :: Parser ()
spaces = skipMany space

parseOperator :: Parser SchExp
parseOperator = liftM Operator $ many1 (oneOf "!#$%&|*+-/:<=>?@^~.")


parseString :: Parser SchExp
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseSymbol :: Parser SchExp
parseSymbol = do
    first <- letter <|> (oneOf "_")
    rest <- many (letter <|> digit <|> (oneOf "_"))
    let symbol = first : rest
    return $ case symbol of
               "true" -> Bool True
               "false" -> Bool False
               _    -> Symbol symbol

parseInt64 :: Parser SchExp
parseInt64 = liftM (Int64 . read) $ many1 digit

parseExpr :: Parser SchExp
parseExpr = do
     spaces
     parseSymbol
     <|> parseOperator
     <|> parseString
     <|> parseInt64
     <|> parseBlock '[' ']' List
     <|> parseBlock '{' '}' Sequence
     <|> parseBlock '(' ')' Tuple


readExpr :: String -> String
readExpr input = case parse parseExpr "schizo" input of
    Left err -> "No match: " ++ show err
    Right exp -> "Found value: " ++ show exp

parseBlock :: Char -> Char -> ([SchExp] -> SchExp) -> Parser SchExp
parseBlock co cc f = do
    char co
    spaces
    x <- {- try (liftM f $ many1 (do { x <- parseApp; char ';'; return x }))
         <|> -} (liftM f $ sepBy parseApp (char ';'))
    spaces
    char cc
    return x

parseApp :: Parser SchExp
parseApp = do
    (h : t) <- many1 parseExpr
    spaces
    return $ case t of
        [] -> h
        _  -> Application (h, t)


