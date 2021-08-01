import Test.Tasty
import Test.Tasty.HUnit
import TestInterpreter (interpreterTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [interpreterTests]