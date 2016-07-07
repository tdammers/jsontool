module Data.JSONTool.AstTransforms
where

import Data.Aeson
import qualified Data.Vector as Vector
import Data.Vector ((!?))
import qualified Data.HashMap.Strict as HashMap
import Data.List (concatMap, sortOn)
import Data.Monoid.Endo
import qualified Data.Text as Text

flatten :: Endo Value
flatten = Endo $ \val -> case go val of
    [] -> Null
    [x] -> x
    xs -> Array $ Vector.fromList xs
    where
        go (Array a) = concatMap go $ Vector.toList a
        go (Object o) = concatMap go $ HashMap.elems o
        go x = [x]

at :: String -> Endo Value
at index = Endo $ \val -> case val of
    Array a -> toJSON . HashMap.lookup index . HashMap.fromList . zip (map show [0..]) . Vector.toList $ a
    Object o -> toJSON . HashMap.lookup (Text.pack index) $ o
    _ -> Null
