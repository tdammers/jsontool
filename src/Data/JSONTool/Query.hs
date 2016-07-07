{-#LANGUAGE LambdaCase #-}
module Data.JSONTool.Query
where

import Data.Aeson
import Data.Monoid
import Data.Monoid.Endo
import Data.Vector ( (!?) )
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

type Query = Endo [Value]

query :: Query -> Value -> Value
query q value = fetchAll $ runQuery [value]

queryOne :: Query -> Value -> Value
queryOne a value = fetchOne $ runQuery [value]

fetchAll :: [Value] -> Value
fetchAll values = Array $ Vector.fromList values

fetchOne :: [Value] -> Value
fetchOne [] = Null
fetchOne (x:xs) = x

runQuery :: Query -> [Value] -> [Value]
runQuery = appEndo

current :: Query
current = mempty

anyChild :: Query
anyChild =
    Endo $ concatMap . resultNodes
    where
        resultNodes :: Value -> [Value]
        resultNodes (Array a) = Vector.toList a
        resultNodes (Object o) = HashMap.elems o
        resultNodes _ = []

childAtKey :: Text -> Query
childAtKey key =
    Endo $ concatMap . resultNodes
    where
        resultNodes :: Value -> [Value]
        resultNodes (Object o) = toJSON . HashMap.lookup key $ o
        resultNodes _ = Null

childAtIndex :: Int -> Query
childAtIndex index =
    Endo $ concatMap . resultNodes
    where
        resultNodes :: Value -> [Value]
        resultNodes (Array a) = toJSON $ a !? index
        resultNodes _ = Null

first :: Query
first =
    Endo $ \case
        [] -> []
        (x:xs) -> [x]
