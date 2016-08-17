{-#LANGUAGE TemplateHaskell #-}
module Main where

import Data.JSONTool
import Data.JSONTool.AstTransforms
import Data.JSONTool.Query.Type
import Data.JSONTool.Query.Parse

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
import Data.FileEmbed

data InvalidArgException = InvalidArgException String
    deriving (Show)

instance Exception InvalidArgException

data CliOptions =
    CliOptions
        { cliPreTrans :: Endo LBS.ByteString
        , cliPostTrans :: Endo LBS.ByteString
        , cliAstTrans :: Endo Value
        , cliPretty :: Bool
        , cliOutputFormat :: OutputFormat
        , cliInputFiles :: Maybe [FilePath]
        , cliShowHelp :: Bool
        }

instance Default CliOptions where
    def = CliOptions mempty mempty mempty False OutputJSON Nothing False

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
    "" -> ([], Endo $ \opts -> opts { cliInputFiles = Just (fromMaybe [] (cliInputFiles opts) ++ args )})
    "help" -> ([], Endo $ \opts -> opts { cliShowHelp = True })
    "-help" -> ([], Endo $ \opts -> opts { cliShowHelp = True })
    "pretty" -> (args, Endo $ \opts -> opts { cliPretty = True })
    "nopretty" -> (args, Endo $ \opts -> opts { cliPretty = False })
    "yml" -> (args, Endo $ \opts -> opts { cliOutputFormat = OutputYaml })
    "yaml" -> (args, Endo $ \opts -> opts { cliOutputFormat = OutputYaml })
    "json" -> (args, Endo $ \opts -> opts { cliOutputFormat = OutputJSON })
    "raw" -> (args, Endo $ \opts -> opts { cliOutputFormat = OutputString })
    "flatten" -> (args, appendAstTrans flatten)
    "first" -> (args, appendAstTrans first)
    "at" -> case args of
        (arg0@('-':_)):_ -> throw $ InvalidArgException (cmd ++ " (invalid parameter " ++ arg0 ++ ")")
        (x:xs) -> (xs, appendAstTrans $ at x)
        _ -> throw $ InvalidArgException (cmd ++ " (missing required parameter)")
    "q" -> case args of
        (arg0@('-':_)):_ -> throw $ InvalidArgException (cmd ++ " (invalid parameter " ++ arg0 ++ ")")
        (x:xs) -> case parseQuery x of
            Right q -> (xs, appendAstTrans . Endo $ query q)
            Left err -> throw $ InvalidArgException ("Syntax error in query: " ++ err)
        _ -> throw $ InvalidArgException (cmd ++ " (missing required parameter)")
    "Q" -> case args of
        (arg0@('-':_)):_ -> throw $ InvalidArgException (cmd ++ " (invalid parameter " ++ arg0 ++ ")")
        (x:xs) -> case parseQuery x of
            Right q -> (xs, appendAstTrans . Endo $ queryExF q)
            Left err -> throw $ InvalidArgException ("Syntax error in query: " ++ err)
        _ -> throw $ InvalidArgException (cmd ++ " (missing required parameter)")
    invalidCmd -> throw $ InvalidArgException cmd
parseArg (fn:remainder) =
    ( remainder
    , Endo $ \opts ->
        opts
            { cliInputFiles = Just (fromMaybe [] (cliInputFiles opts) ++ [fn]) })

showHelp :: IO ()
showHelp = putStrLn $(embedStringFile "HELP")


appendAstTrans :: Endo Value -> Endo CliOptions
appendAstTrans trans = Endo $ \opts -> opts { cliAstTrans = trans <> cliAstTrans opts }

main :: IO ()
main = do
    args <- getArgs
    let opts = parseArgs args
    if cliShowHelp opts
        then
            showHelp
        else do
            let goFile :: Maybe FilePath -> IO ()
                goFile fpm = do
                    inHandle <- maybe
                        (return stdin)
                        (flip openFile ReadMode)
                        fpm
                    process
                        (cliPreTrans opts)
                        (cliAstTrans opts)
                        (cliPostTrans opts)
                        (cliPretty opts)
                        (cliOutputFormat opts)
                        inHandle
                        stdout
            maybe (goFile Nothing) (mapM_ (goFile . Just)) (cliInputFiles opts)
