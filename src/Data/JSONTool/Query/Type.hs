{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE OverloadedStrings #-}
module Data.JSONTool.Query.Type
where

import Data.Aeson
import Data.Monoid
import Data.Monoid.Endo
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text

data MatchKey = IntKey Int | StringKey Text

instance ToJSON MatchKey where
    toJSON (IntKey i) = toJSON i
    toJSON (StringKey s) = toJSON s

data Match = Match { matchPath :: [MatchKey], matchValue :: Value }

instance ToJSON Match where
    toJSON (Match path value) = Array [ toJSON path, value ]

type Query = Endo [Match]

queryWith :: (Match -> a) -> ([a] -> Value) -> Query -> Value -> Value
queryWith mapper folder q value =  folder . map mapper $ runQuery q [Match [] value]

query :: Query -> Value -> Value
query = queryWith matchValue toJSON

queryEx :: Query -> Value -> Value
queryEx = queryWith toJSON toJSON

queryExF :: Query -> Value -> Value
queryExF = queryWith formatMatch object

formatMatch :: Match -> (Text, Value)
formatMatch match =
    ( Text.intercalate "/" . map keyToStr $ matchPath match
    , matchValue match
    )
    where
        keyToStr (IntKey i) = Text.pack $ show i
        keyToStr (StringKey s) = s


runQuery :: Query -> [Match] -> [Match]
runQuery = appEndo

