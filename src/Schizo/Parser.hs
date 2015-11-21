module Schizo.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Map
import Schizo.Expression


spaces :: Parser ()
spaces = skipMany space

parseOperator :: Parser SchExp
parseOperator = liftM Operator $ many1 (oneOf "!#$%&|*+-/:<=>?@^~.")

parseString :: Parser SchExp
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
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
parseInt64 = do
    sign <- try (oneOf "+-") <|> return '+'
    int <- many1 digit
    return $ Int64 $ let ri = read int in if sign == '-' then - ri else ri

-- parse a floating point number
-- D			[0-9]
-- E			[Ee][+-]?{D}+
-- [+-]?{D}+"."{D}*({E})?
-- [+-]?{D}+"."{D}*
parseFloat64 :: Parser SchExp
parseFloat64 = liftM (Float64 . read) $
    do  sign    <- try (oneOf "+-") <|> return '+'
        intPart <- many1 digit
        _       <- char '.'
        decPart <- many digit
        try $ do
                e       <- char 'E' <|> char 'e'
                sign    <- char '+' <|> char '-'
                expPart <- many1 digit
                return $ intPart ++ '.' : decPart ++ e : sign : expPart
            <|> (return $ intPart ++ '.' : decPart)

parseExpr :: Parser SchExp
parseExpr = do
     spaces >>
         ((try parseFloat64 <|> parseInt64)
         <|> parseSymbol
         <|> parseOperator
         <|> parseString
         <|> parseBlock '[' ']' ',' List
         <|> parseBlock '{' '}' ';' Sequence
         <|> parseBlock '(' ')' ',' Tuple) <* spaces

parseBlock :: Char -> Char -> Char -> ([SchExp] -> SchExp) -> Parser SchExp
parseBlock co cc sep f = do
    _   <- char co
    x   <- (liftM f $ sepBy parseApp (char sep))
    _   <- char cc
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
