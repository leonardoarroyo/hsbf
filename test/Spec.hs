import Test.Tasty
import Test.Tasty.HUnit
import TestInterpreter (interpreterUnitTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [interpreterUnitTests]