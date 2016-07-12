module Data.JSONTool.Query.Parse
where

import Text.Parsec
import Data.JSONTool.Query.Type
import Data.JSONTool.Query.Queries
import Data.Monoid

parseQuery :: String -> Either String Query
parseQuery str =
    let result = runParser queryP () "" str
    in case result of
        Left err -> Left $ show err
        Right q -> Right q

queryP :: Parsec String () Query
queryP = mconcat <$> sepBy1 pathItemP (string "/")

pathItemP :: Parsec String () Query
pathItemP = baseItemP

baseItemP :: Parsec String () Query
baseItemP = anyChildP <|> currentP <|> implicitCurrentP

anyChildP :: Parsec String () Query
anyChildP = string "*" >> return anyChild

currentP :: Parsec String () Query
currentP = string "." >> return current

implicitCurrentP :: Parsec String () Query
implicitCurrentP = return current
