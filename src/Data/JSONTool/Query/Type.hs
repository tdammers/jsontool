module Data.JSONTool.Query.Type
where

import Data.Aeson
import Data.Monoid
import Data.Monoid.Endo
import qualified Data.Vector as Vector

type Query = Endo [Value]

query :: Query -> Value -> Value
query q value = fetchAll $ runQuery q [value]

queryOne :: Query -> Value -> Value
queryOne q value = fetchOne $ runQuery q [value]

fetchAll :: [Value] -> Value
fetchAll values = Array $ Vector.fromList values

fetchOne :: [Value] -> Value
fetchOne [] = Null
fetchOne (x:xs) = x

runQuery :: Query -> [Value] -> [Value]
runQuery = appEndo

