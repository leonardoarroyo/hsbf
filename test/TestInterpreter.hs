{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInterpreter where

import qualified Data.Map as M
import Ast
import Data.List
import Data.Maybe
import Interpreter
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Data.Word

interpreterUnitTests =
  testGroup
    "Interpreter"
    [ testGroup
        "newProgram"
        [ QC.testProperty "newProgram x == ProgramState newTape 0 x []" $
            \x -> newProgram x == ProgramState newTape 0 x []
        ],
      testGroup
        "headStatement"
        [ QC.testProperty
            "headStatement x == head x when x is not empty"
            $ QC.forAll (QC.listOf1 (arbitrary :: Gen Stmt)) $ \x -> headStatement x == head x,
          testCase
            "headStatement [] == Exit"
            $ headStatement [] @?= Exit
        ],
      testGroup
        "getTapeCell"
        [ QC.testProperty
            "getTapeCell tape ptr returns Just element at index ptr when element is defined"
            $ QC.forAll (arbitrary :: Gen Int) $ \x -> getTapeCell indexIsCellTape x == Just (fromIntegral x),
          QC.testProperty
            "getTapeCell tape ptr returns Nothing when index ptr is not defined"
            $ QC.forAll (arbitrary :: Gen Int) $ \x -> isNothing (getTapeCell newTape x)
        ],
      testGroup
        "putTapeCell"
        [ QC.testProperty "getTapeCell (putTapeCell tape ptr value) ptr == Just value"
            $ QC.forAll (arbitrary :: Gen (Int, Word8))
            $ \(ptr, val) -> getTapeCell (putTapeCell indexIsCellTape ptr val) ptr == Just val
        ]
    ]

indexIsCellTape :: Tape
indexIsCellTape = M.fromList [(x, fromIntegral x) | x <- [-15000..15000]]

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