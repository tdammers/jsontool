module Data.JSONTool
( IsSource (..)
, IsSink (..)
, process
)
where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Monoid
import Data.Monoid.Endo
import System.IO

class IsSource a where
    getBytes :: a -> IO LBS.ByteString

instance IsSource LBS.ByteString where
    getBytes = return

instance IsSource Handle where
    getBytes = LBS.hGetContents


class IsSink a where
    putBytes :: a -> LBS.ByteString -> IO ()

instance IsSink Handle where
    putBytes = LBS.hPut


process :: (IsSource source, IsSink sink)
        => Endo LBS.ByteString -- ^ pre-processor run on input JSON source
        -> Endo Value -- ^ AST processor run on parsed JSON
        -> Endo LBS.ByteString -- ^ post-processor run on output JSON source
        -> Bool -- ^ whether to apply pretty-printing
        -> source -- ^ where to read input from
        -> sink -- ^ where to write output to
        -> IO ()
process preTrans astTrans postTrans pretty source sink = do
    src <- appEndo preTrans <$> getBytes source
    jsonIn <- either fail return $ eitherDecode src
    let jsonOut = appEndo astTrans jsonIn
        encoder = if pretty then encodePretty else encode
    let dst = encoder jsonOut
    putBytes sink $ appEndo postTrans dst
