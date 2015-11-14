--
-- This will be rewritten in parsec after. Now it's done the hard way for learning purposes
--
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

--------------------------------------------------------------------------------
-- one or more element (A+)
--------------------------------------------------------------------------------
oneOrMore   :: Predicate a
            -> Match a
            -> Match a

oneOrMore pa stream =
    case (pa, stream) of
        (Predicate pa, Match (tokenList, h:charList)) | pa h ->
            let (tok, cList) = loop [h] charList
            in  Match (tok : tokenList, cList)
            where loop tok cList =
                    case cList of
                       h : t | pa h -> loop (h : tok) t
                       _ -> ((Token (reverse tok)), cList)
        (Predicate pa, Match (tokenList, charList)) -> Unmatched (tokenList, charList)
        (_, _) -> error "trying to process unmatched stream"

--------------------------------------------------------------------------------
-- Zero or more element (A*)
--------------------------------------------------------------------------------
zeroOrMore  :: Predicate a
            -> Match a
            -> Match a

zeroOrMore pa stream =
    case (pa, stream) of
        (Predicate pa, Match (tokenList, charList)) ->
            let (tok, cList) = loop [] charList
            in  Match (tok : tokenList, cList)
            where loop tok cList =
                    case cList of
                       h : t | pa h -> loop (h : tok) t
                       _ -> ((Token (reverse tok)), cList)
        (_, _) -> error "trying to process unmatched stream"

--------------------------------------------------------------------------------
-- exact string
--------------------------------------------------------------------------------
exactList :: Eq a
          => [a]
          -> Match a
          -> Match a

exactList xs stream =
    let xsLen = length xs
    in case stream of
        Match (tokenList, charList) | xsLen <= length charList && take (length xs) charList == xs ->
            Match (Token xs : tokenList, drop xsLen charList)
        Match (tokenList, charList) -> Unmatched (tokenList, charList)
        Unmatched _ -> error "trying to process unmatched stream"


--------------------------------------------------------------------------------
-- unit test
--------------------------------------------------------------------------------
parserTests = do
    let str = "12345678abcde"

    putStrLn "One or More..."
    let am1 = oneOrMore digit (Match ([], str))
    let am2 = oneOrMore alpha am1
    putStrLn (show am1)
    putStrLn (show am2)

    let am1 = oneOrMore alpha (Match ([], str))
    let am2 = oneOrMore digit am1
    putStrLn (show am1)
    -- this will fail putStrLn (show am2)

    putStrLn "Zero or More..."
    let am1 = zeroOrMore digit (Match ([], str))
    let am2 = zeroOrMore alpha am1
    putStrLn (show am1)
    putStrLn (show am2)

    let am1 = zeroOrMore alpha (Match ([], str))
    let am2 = zeroOrMore digit am1
    putStrLn (show am1)
    putStrLn (show am2)

    putStrLn "Exact list..."
    let am1 = exactList "123456" (Match ([], str))
    let am2 = exactList "124446" (Match ([], str))
    putStrLn (show am1)
    putStrLn (show am2)
