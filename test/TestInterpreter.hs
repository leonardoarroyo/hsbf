{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInterpreter where

import Ast
import Data.List
import Interpreter
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

prop_hSx_eq_hx x = headStatement x == head x

prop_Myfunc :: [Int] -> (Int, Int) -> Bool
prop_Myfunc ints (i, j) = ints !! i == ints !! j

interpreterUnitTests =
  testGroup
    "Interpreter"
    [ testGroup
        "newProgram"
        [ QC.testProperty "newProgram x == ProgramState newTape 0 x []" $
            \x -> newProgram x == ProgramState newTape 0 x []
        ],
      testGroup
        "newTape"
        [ testCase "newTape == replicate 100 0" $
            newTape @?= replicate 100 0
        ],
      testGroup
        "headStatement"
        [ QC.testProperty
            "headStatement x == head x when x is not empty"
            $ QC.forAll (QC.listOf1 (arbitrary :: Gen Stmt)) $ \x -> headStatement x == head x,
          testCase
            "headStatement [] == Exit"
            $ headStatement [] @?= Exit
        ]
        --testGroup
        --  "putTapeCell"
        --  [ QC.testProperty "(putTapeCell tape ptr value) !! ptr == value" $
        --      \x -> newProgram x == ProgramState (replicate 100 0) 0 x []
        --  ]
    ]

instance Arbitrary Stmt where
  arbitrary = elements [Increment, Decrement, MoveRight, MoveLeft, CharIn, CharOut]

--nonEmptyStmts :: Gen [Stmt]
--nonEmptyStmts = resize 10 flexList
--
--flexList :: Arbitrary a => Gen [a]
--flexList = sized $ \n ->
--  frequency
--    [ (1, return []),
--      (n, (:) <$> arbitrary <*> flexList)
--    ]
--
--nonEmptyFlexList :: Arbitrary a => Gen [a]
--nonEmptyFlexList = sized $ \n ->
--  frequency
--    [ (1, arbitrary),
--      (n, (:) <$> arbitrary <*> nonEmptyFlexList)
--    ]
--
--genArgs :: Gen ([Int], Int, Int)
--genArgs = do
--  x <- arbitrary
--  xs <- arbitrary
--  let n = length xs
--  i <- choose (0, n)
--  j <- choose (0, n)
--  return ((x : xs), i, j) -- return a non-empty list
--  test noops inside loops