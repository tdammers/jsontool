{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Data.JSONTool.Query.QueriesTest
where

import Data.JSONTool.Query
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Monoid

queriesTests = testGroup "queries"
    [ currentNodeTest
    , allChildrenTest
    , allDescendantsTest
    , childByKeyTest
    , queryMonoidTests
    ]

currentNodeTest = testCase "current node selector" $ do
    let q = current
        input = Array ["foo", "bar"]
        actual = query q input
        expected = Array [ Array ["foo", "bar"] ]
    assertEqual "" expected actual

allChildrenTest = testCase "all children selector" $ do
    let q = anyChild
        input = Array ["foo", "bar"]
        actual = query q input
        expected = Array ["foo", "bar"]
    assertEqual "" expected actual

allDescendantsTest = testCase "all descendants selector" $ do
    let q = anyDescendant
        input = Array [ Array [ "foo", "bar" ], "baz" ]
        actual = query q input
        expected = Array [ Array ["foo", "bar"], "baz", "foo", "bar" ]
    assertEqual "" expected actual

childByKeyTest = testCase "children by key" $ do
    let q = childAtKey "foo"
        input = object [ ("foo", "bar"), ("baz", "quux")]
        actual = query q input
        expected = Array ["bar"]
    assertEqual "" expected actual


queryMonoidTests = testGroup "monoid of queries" 
    [ testCase "*/*" $ do
        let q = anyChild <> anyChild
            input = Array ["foo", Array [ "bar"] ]
            actual = query q input
            expected = Array ["bar"]
        assertEqual "" expected actual
    , testCase "foo/bar" $ do
        let q = childAtKey "bar" <> childAtKey "foo"
            input = object
                [ ("foo", object
                    [ ("bar", "baz") ]
                  )
                ]
            actual = query q input
            expected = Array ["baz"]
        assertEqual "" expected actual
    ]
