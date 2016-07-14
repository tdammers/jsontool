module Data.JSONTool
( IsSource (..)
, IsSink (..)
, OutputFormat (..)
, process
)
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Monoid
import Data.Monoid.Endo
import System.IO
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml

data OutputFormat = OutputJSON | OutputYaml

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
        -> OutputFormat -- ^ whether to output JSON or YAML
        -> source -- ^ where to read input from
        -> sink -- ^ where to write output to
        -> IO ()
process preTrans astTrans postTrans pretty outfmt source sink = do
    src <- appEndo preTrans <$> getBytes source
    jsonIn <- either fail return $ Yaml.decodeEither (LBS.toStrict src)
    let jsonOut = appEndo astTrans jsonIn
        encoder = case (outfmt, pretty) of
            (OutputJSON, True) -> encodePretty
            (OutputJSON, False) -> encode
            (OutputYaml, True) -> LBS.fromStrict . Yaml.encodePretty Yaml.defConfig
            (OutputYaml, False) -> LBS.fromStrict . Yaml.encode
    let dst = encoder jsonOut
    putBytes sink $ appEndo postTrans dst
