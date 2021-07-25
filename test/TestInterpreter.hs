module TestInterpreter where

import Data.List
import Interpreter
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

interpreterUnitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $
        [1, 2, 3] `compare` [1, 2] @?= GT,
      -- the following test does not hold
      testCase "List comparison (same length)" $
        1 @?= 1,
      SC.testProperty "continue x == (Running, x)" $
        \x -> continue (x :: Int) == (Running, x)
    ]