module Schizo.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
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
parseInt64 = liftM (Int64 . read) $ do {
    (char '+' >> many1 digit)
    <|> many1 digit
    <|> (do
        x <- char '-'
        y <- many1 digit
        return $ x : y) }

parseExpr :: Parser SchExp
parseExpr = do
     spaces >>
         (parseInt64
         <|> parseBlock '[' ']' ',' List
         <|> parseBlock '{' '}' ';' Sequence
         <|> parseBlock '(' ')' ',' Tuple
         <|> parseSymbol
         <|> parseOperator
         <|> parseString) <* spaces

parseBlock :: Char -> Char -> Char -> ([SchExp] -> SchExp) -> Parser SchExp
parseBlock co cc sep f = do
    char co
    x <- (liftM f $ sepBy parseApp (char sep))
    char cc
    return x

parseApp :: Parser SchExp
parseApp = do
    (h : t) <- many1 parseExpr
    return $ case t of
        [] -> h
        _  -> Application (h, t)

readExpr :: String -> String
readExpr input = case parse parseExpr "schizo" input of
    Left err -> "No match: " ++ show err
    Right v -> "Found value: " ++ show v

