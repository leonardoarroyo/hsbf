import Test.Tasty
import Test.Tasty.HUnit
import TestInterpreter (interpreterUtilsTests, interpreterVMTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [interpreterUtilsTests, interpreterVMTests]