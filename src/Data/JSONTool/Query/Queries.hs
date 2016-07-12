{-#LANGUAGE LambdaCase #-}
module Data.JSONTool.Query.Queries
where

import Data.Aeson
import Data.Monoid
import Data.Monoid.Endo
import Data.Vector ( (!?) )
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.JSONTool.Query.Type

current :: Query
current = mempty

anyChild :: Query
anyChild =
    Endo $ concatMap resultNodes
    where
        resultNodes :: Value -> [Value]
        resultNodes (Array a) = Vector.toList a
        resultNodes (Object o) = HashMap.elems o
        resultNodes _ = []

childAtKey :: Text -> Query
childAtKey key =
    Endo $ concatMap resultNodes
    where
        resultNodes :: Value -> [Value]
        resultNodes (Object o) = maybe [] (:[]) $ HashMap.lookup key o
        resultNodes _ = []

childAtIndex :: Int -> Query
childAtIndex index =
    Endo $ concatMap resultNodes
    where
        resultNodes :: Value -> [Value]
        resultNodes (Array a) = [toJSON $ a !? index]
        resultNodes _ = []

first :: Query
first =
    Endo $ \case
        [] -> []
        (x:xs) -> [x]

having :: Query -> Query
having q =
    Endo $ filter isMatch
    where
        isMatch :: Value -> Bool
        isMatch = not . null . runQuery q . (:[])

