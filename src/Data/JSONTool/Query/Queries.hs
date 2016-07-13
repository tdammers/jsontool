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
        resultNodes :: Match -> [Match]
        resultNodes (Match p (Array a)) =
            zipWith Match ([p ++ [IntKey i] | i <- [0..]]) (Vector.toList a)
        resultNodes (Match p (Object o)) =
            [Match (p ++ [StringKey k]) v | (k, v) <- HashMap.toList o]
        resultNodes _ = []

anyDescendant :: Query
anyDescendant =
    Endo $ concatMap resultNodes
    where
        resultNodes :: Match -> [Match]
        resultNodes val =
            let children = runQuery anyChild [val]
            in children ++ concatMap (runQuery anyDescendant . (:[])) children

childAtKey :: Text -> Query
childAtKey key =
    Endo $ concatMap resultNodes
    where
        resultNodes :: Match -> [Match]
        resultNodes (Match p (Object o)) =
            maybe [] (:[]) $ Match (p ++ [StringKey key]) <$> HashMap.lookup key o
        resultNodes _ = []

childAtIndex :: Int -> Query
childAtIndex index =
    Endo $ concatMap resultNodes
    where
        resultNodes :: Match -> [Match]
        resultNodes (Match p (Array a)) =
            let foundMay = a !? index
            in maybe [] (\found -> [Match (p ++ [IntKey index]) found]) foundMay
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
        isMatch :: Match -> Bool
        isMatch = not . null . runQuery q . (:[])

alternative :: [Query] -> Query
alternative queries = Endo $ concatMap resultNodes
    where
        resultNodes :: Match -> [Match]
        resultNodes val = concatMap (\q -> runQuery q [val]) queries
