module Data.JSONTool.AstTransforms
where

import Data.Aeson
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.List (concatMap)

flatten :: Value -> Value
flatten val = case go val of
    [] -> Null
    [x] -> x
    xs -> Array $ Vector.fromList xs
    where
        go (Array a) = concatMap go $ Vector.toList a
        go (Object o) = concatMap go $ HashMap.elems o
        go x = [x]
