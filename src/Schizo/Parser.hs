module Schizo.Parser where

data Predicate a = Predicate (a -> Bool)

Predicate a ||| Predicate b = Predicate (\x -> a x || b x)
Predicate a &&& Predicate b = Predicate (\x -> a x && b x)

data Token a = Token [a] deriving Show

data Match a = Match     ([Token a], [a])
             | Unmatched ([Token a], [a])
             deriving Show

digit =
    let isDigit x =
         case x of
            x | x >= '0' && x <= '9' -> True
            _ -> False
    in Predicate isDigit

alphaUp =
    let isAlphaUp x =
          case x of
             x | x >= 'A' && x <= 'Z' -> True
             _ -> False
    in Predicate isAlphaUp

alphaLow =
    let isAlphaLow x =
         case x of
            x | x >= 'a' && x <= 'z' -> True
            _ -> False
    in Predicate isAlphaLow

alpha = alphaUp ||| alphaLow

isChar  :: Char
        -> Predicate Char

isChar c = Predicate (\x -> x == c)

oneOrMore   :: a
            -> Match a
            -> Match a

oneOrMore a stream =
    case stream of
        Match (tokenList, charList) ->
            Match (tokenList, charList)
        Unmatched _ -> stream




