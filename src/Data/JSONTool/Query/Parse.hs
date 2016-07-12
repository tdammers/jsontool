module Data.JSONTool.Query.Parse
where

import Text.Parsec
import Data.JSONTool.Query.Type
import Data.JSONTool.Query.Queries
import Data.Monoid
import qualified Data.Text as Text
import Data.Char (isAlpha, isDigit)

parseQuery :: String -> Either String Query
parseQuery str =
    let result = runParser (queryP <* eof) () "" str
    in case result of
        Left err -> Left $ show err
        Right q -> Right q

queryP :: Parsec String () Query
queryP = pathP

pathP :: Parsec String () Query
pathP = mconcat . reverse <$> sepBy1 pathItemP (string "/")

pathItemP :: Parsec String () Query
pathItemP = alternativeItemP

alternativeItemP :: Parsec String () Query
alternativeItemP = alternative <$> sepBy1 qualifiedItemP (string "|")

qualifiedItemP :: Parsec String () Query
qualifiedItemP = do
    base <- baseItemP
    qualifiers <- many (try qualifierP)
    return $ (mconcat . reverse) (base:qualifiers)

qualifierP :: Parsec String () Query
qualifierP = fmap having $ string "[" *> queryP <* string "]"

baseItemP :: Parsec String () Query
baseItemP = groupedP
          <|> childByIndexP
          <|> childByNameP
          <|> anyDescendantP
          <|> anyChildP
          <|> currentP
          <|> implicitCurrentP

groupedP :: Parsec String () Query
groupedP = string "(" *> queryP <* string ")"

childByNameP :: Parsec String () Query
childByNameP = do
    h <- satisfy isAlpha
    t <- many (alphaNum <|> oneOf "-_")
    return . childAtKey . Text.pack $ h:t

childByIndexP :: Parsec String () Query
childByIndexP = childAtIndex <$> intP

intP :: (Read a, Num a, Integral a) => Parsec String () a
intP = zeroP <|> posIntP

zeroP :: (Read a, Num a) => Parsec String () a
zeroP = string "0" >> return 0

posIntP :: (Read a, Integral a) => Parsec String () a
posIntP = do
    h <- satisfy (`elem` ['1'..'9'])
    t <- many (satisfy isDigit)
    return . read $ h:t

anyChildP :: Parsec String () Query
anyChildP = string "*" >> return anyChild

anyDescendantP :: Parsec String () Query
anyDescendantP = try $ string "**" >> return anyDescendant

currentP :: Parsec String () Query
currentP = string "." >> return current

implicitCurrentP :: Parsec String () Query
implicitCurrentP = return current
