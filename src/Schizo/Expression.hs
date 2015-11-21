module Schizo.Expression where

import Data.Map

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

data Environment =
    Environment { operators  :: Map String Int       -- infix operators
                , impMacros  :: Map String SchExp    -- imported macros
                , privMacros :: Map String SchExp    -- private macros
                , expMacros  :: Map String SchExp    -- exported macros
                }
    deriving Show

eval    :: Environment
        -> SchExp
        -> SchExp
eval env e =
    error "not implemented yet"

