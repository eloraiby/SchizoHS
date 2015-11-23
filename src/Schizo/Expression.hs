module Schizo.Expression where

import Data.Map as Map

data SchExp  = Symbol       String
             | Operator     String
             | List         [SchExp]
             | Tuple        [SchExp]
             | Sequence     [SchExp]
             | Application  (SchExp, [SchExp])
             | Int64        Int
             | Float64      Double
             | String       String
             | Bool         Bool
             deriving (Eq, Show)

data Environment =
    Environment { operators  :: Map String Int       -- infix operators
                , impMacros  :: Map String SchExp    -- imported macros
                , privMacros :: Map String SchExp    -- private macros
                , expMacros  :: Map String SchExp    -- exported macros
                }
    deriving Show

infixl 9 ?>?
(?>?) :: Maybe a -> Maybe a -> Maybe a

Just x  ?>? _ = Just x
Nothing ?>? y = y

sxLookup  :: String
        -> Environment
        -> Maybe SchExp

sxLookup s e =   Map.lookup s (expMacros e)
             ?>? Map.lookup s (privMacros e)
             ?>? Map.lookup s (impMacros e)
             ?>? case Map.lookup s (operators e) of
                     Just i -> Just $ Operator s
                     Nothing -> Nothing

{-
-- Macro evaluation
-}
eval    :: Environment
        -> SchExp
        -> SchExp

eval env e =
    case e of
        Symbol s ->
            case sxLookup s env of
                Just v -> eval env v
                Nothing -> e    -- return the same symbol (unresolved)

        Application  (h, args) -> apply env h args
        _ -> e

apply   :: Environment
        -> SchExp
        -> [SchExp]
        -> SchExp

apply env e args = error "not implemented"




