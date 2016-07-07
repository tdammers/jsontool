module Main where

import Data.JSONTool
import Data.JSONTool.AstTransforms

import System.IO (stdin, stdout, openFile, IOMode (..))
import System.Environment (getArgs)
import Data.Monoid
import Data.Monoid.Endo (Endo)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (..))
import Control.Exception
import Control.Monad (mapM_)
import Data.Maybe (fromMaybe)

data InvalidArgException = InvalidArgException String
    deriving (Show)

instance Exception InvalidArgException

data CliOptions =
    CliOptions
        { cliPreTrans :: Endo LBS.ByteString
        , cliPostTrans :: Endo LBS.ByteString
        , cliAstTrans :: Endo Value
        , cliPretty :: Bool
        , cliInputFiles :: Maybe [FilePath]
        }

instance Default CliOptions where
    def = CliOptions mempty mempty mempty False Nothing

parseArgs :: [String] -> CliOptions
parseArgs xs = appEndo (mconcat . reverse $ go xs) def
    where
        go :: [String] -> [Endo CliOptions]
        go [] = []
        go xs =
            let (remainder, f) = parseArg xs
            in f:(go remainder)

parseArg :: [String] -> ([String], Endo CliOptions)
parseArg [] = ([], mempty)
parseArg (('-':cmd):args) = case cmd of
    "pretty" -> (args, Endo $ \opts -> opts { cliPretty = True })
    "nopretty" -> (args, Endo $ \opts -> opts { cliPretty = False })
    "flatten" -> (args, appendAstTrans flatten)
    "at" -> case args of
        (arg0@('-':_)):_ -> throw $ InvalidArgException (cmd ++ " (invalid parameter " ++ arg0 ++ ")")
        (x:xs) -> (xs, appendAstTrans $ at x)
        _ -> throw $ InvalidArgException (cmd ++ " (missing required parameter)")
    invalidCmd -> throw $ InvalidArgException cmd
parseArg (fn:remainder) =
    ( remainder
    , Endo $ \opts ->
        opts
            { cliInputFiles = Just (fromMaybe [] (cliInputFiles opts) ++ [fn]) })

appendAstTrans :: Endo Value -> Endo CliOptions
appendAstTrans trans = Endo $ \opts -> opts { cliAstTrans = cliAstTrans opts <> trans }

main :: IO ()
main = do
    args <- getArgs
    let opts = parseArgs args
    let goFile :: Maybe FilePath -> IO ()
        goFile fpm = do
            inHandle <- maybe (return stdin) (flip openFile ReadMode) fpm
            process
                (cliPreTrans opts)
                (cliAstTrans opts)
                (cliPostTrans opts)
                (cliPretty opts)
                inHandle
                stdout
    maybe (goFile Nothing) (mapM_ (goFile . Just)) (cliInputFiles opts)
