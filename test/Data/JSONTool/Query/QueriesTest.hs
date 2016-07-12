{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Data.JSONTool.Query.QueriesTest
where

import Data.JSONTool.Query
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson

queriesTests = testGroup "queries"
    [ currentNodeTest
    , allChildrenTest
    ]

currentNodeTest = testCase "current node selector" $ do
    let q = current
        input = Array ["foo", "bar"]
        actual = query q input
        expected = Array [ Array ["foo", "bar"] ]
    assertEqual "" actual expected

allChildrenTest = testCase "all children selector" $ do
    let q = anyChild
        input = Array ["foo", "bar"]
        actual = query q input
        expected = Array ["foo", "bar"]
    assertEqual "" actual expected
