{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Data.JSONTool.Query.ParseTest
where

import Data.JSONTool.Query
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson

queryParserTests = testGroup "query parser"
    [ parseCurrentNodeTest
    , parseAllChildrenTest
    ]

parseCurrentNodeTest = testCase "current node selector" $ do
    let querySrc = "."
    q <- eitherFail $ parseQuery querySrc
    let input = Array [ "a", "b" ]
        expected = Array [ Array ["a", "b"] ]
        actual = query q input
    assertEqual "" actual expected

parseAllChildrenTest = testGroup "all children selector"
    [ testCase "of array" $ do
        let querySrc = "*"
        q <- eitherFail $ parseQuery querySrc
        let input = Array [ "a", "b" ]
            expected = Array ["a", "b"]
            actual = query q input
        assertEqual "" actual expected
    , testCase "of object" $ do
        let querySrc = "*"
        q <- eitherFail $ parseQuery querySrc
        let input = object [ ("foo", "a"), ("bar", "b") ]
            expected = Array ["a", "b"]
            actual = query q input
        assertEqual "" actual expected
    , testCase "of string" $ do
        let querySrc = "*"
        q <- eitherFail $ parseQuery querySrc
        let input = "nope"
            expected = Array []
            actual = query q input
        assertEqual "" actual expected
    ]

eitherFail :: (Monad m, Show e) => Either e a -> m a
eitherFail (Left e) = fail (show e)
eitherFail (Right a) = return a

resultFail :: (Monad m) => Result a -> m a
resultFail (Success x) = return x
resultFail (Error e) = fail e
