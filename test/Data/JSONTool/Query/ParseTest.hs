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
    , parsePathTest
    , parseHasTest
    ]

parseCurrentNodeTest = testCase "current node selector" $ do
    let querySrc = "."
    q <- eitherFail $ parseQuery querySrc
    let input = Array [ "a", "b" ]
        expected = Array [ Array ["a", "b"] ]
        actual = query q input
    assertEqual "" expected actual

parseAllChildrenTest = testGroup "all children selector"
    [ testCase "of array" $ do
        let querySrc = "*"
        q <- eitherFail $ parseQuery querySrc
        let input = Array [ "a", "b" ]
            expected = Array ["a", "b"]
            actual = query q input
        assertEqual "" expected actual
    , testCase "of object" $ do
        let querySrc = "*"
        q <- eitherFail $ parseQuery querySrc
        let input = object [ ("foo", "a"), ("bar", "b") ]
            expected = Array ["a", "b"]
            actual = query q input
        assertEqual "" expected actual
    , testCase "of string" $ do
        let querySrc = "*"
        q <- eitherFail $ parseQuery querySrc
        let input = "nope"
            expected = Array []
            actual = query q input
        assertEqual "" expected actual
    ]

parsePathTest = testGroup "path with exact matches"
    [ testCase "by keys" $ do
        let querySrc = "foo/bar"
        q <- eitherFail $ parseQuery querySrc
        let input = object
                [ ("foo", object
                    [ ("bar", object
                        [ ("quux", "hi") ]
                      )
                    ]
                  )
                , ("quux", "hello")
                , ("bar", "nope")
                ]
            expected = Array
                [ object [ ("quux", "hi") ] ]
            actual = query q input
        assertEqual "" expected actual
    , testCase "by integer indexes" $ do
        let querySrc = "0/1"
        q <- eitherFail $ parseQuery querySrc
        let input = Array
                [ Array
                    [ "nope"
                    , object [ ("quux", "hi") ]
                    ]
                , "neither"
                ]
            expected = Array
                [ object [ ("quux", "hi") ] ]
            actual = query q input
        assertEqual "" expected actual
    ]

parseHasTest = testCase "has child" $ do
    let querySrc = "foo[bar]"
    q <- eitherFail $ parseQuery querySrc
    let input = object
            [ ("foo", object
                [ ("bar", object
                    [ ("quux", "hi")
                    ]
                  )
                ]
              )
            , ("quux", "hello")
            , ("bar", "nope")
            ]
        expected = Array
            [ object
                [ ("bar", object
                    [ ("quux", "hi")
                    ]
                  )
                ]
            ]
        actual = query q input
    assertEqual "" expected actual

eitherFail :: (Monad m, Show e) => Either e a -> m a
eitherFail (Left e) = fail (show e)
eitherFail (Right a) = return a

resultFail :: (Monad m) => Result a -> m a
resultFail (Success x) = return x
resultFail (Error e) = fail e
