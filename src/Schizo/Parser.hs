module Schizo.Parser where

data Predicate a = Predicate (a -> Bool)

Predicate a ||| Predicate b = Predicate (\x -> a x || b x)
Predicate a &&& Predicate b = Predicate (\x -> a x && b x)

data Token a = Token [a]

data Match a = Match     ([Token a], [a])
             | Unmatched ([Token a], [a])




