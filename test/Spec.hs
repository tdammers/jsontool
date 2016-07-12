import Test.Tasty
import Data.JSONTool.Query.ParseTest
import Data.JSONTool.Query.QueriesTest

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "*"
    [ queryParserTests
    , queriesTests
    ]
