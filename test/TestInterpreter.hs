{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInterpreter where

import Ast
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Interpreter
import Safe
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

interpreterUnitTests =
  testGroup
    "Interpreter"
    [ testGroup
        "newProgram"
        [ testProperty "newProgram x == ProgramState newTape 0 x []" $
            \x -> newProgram x == ProgramState newTape 0 x []
        ],
      testGroup
        "headStatement"
        [ testProperty
            "headStatement x == head x when x is not empty"
            $ forAll (listOf1 (arbitrary :: Gen Stmt)) $ \x -> headStatement x == head x,
          testCase
            "headStatement [] == Exit"
            $ headStatement [] @?= Exit
        ],
      testGroup
        "getTapeCell"
        [ testProperty
            "getTapeCell tape ptr returns Just element at index ptr when element is defined"
            $ forAll (arbitrary :: Gen Int) $ \x -> getTapeCell indexIsCellTape x == Just (fromIntegral x),
          testProperty
            "getTapeCell tape ptr returns Nothing when index ptr is not defined"
            $ forAll (arbitrary :: Gen Int) $ \x -> isNothing (getTapeCell newTape x)
        ],
      testGroup
        "putTapeCell"
        [ testProperty "getTapeCell (putTapeCell tape ptr value) ptr == Just value" $
            forAll (arbitrary :: Gen (Int, Word8)) $
              \(ptr, val) -> getTapeCell (putTapeCell indexIsCellTape ptr val) ptr == Just val
        ],
      testGroup
        "getCell"
        [ testProperty "getCell s == 0 when index at ptr is undefined" $
            forAll (programStateWithTape newTape) $
              \s -> getCell s == 0,
          testProperty "(getCell s) returns element at index (ptr s) on (tape s) when index at ptr is defined" $
            forAll (programStateWithTape indexIsCellTape) $
              \s -> Just (getCell s) == M.lookup (ptr s) (tape s)
        ],
      testGroup
        "consumeProg"
        [ testProperty "consumeProg s drops (prog s) head" $
            forAll (arbitrary :: Gen ProgramState) $
              \s -> consumeProg s == s {prog = tailSafe (prog s)}
        ]
    ]

indexIsCellTape :: Tape
indexIsCellTape = M.fromList [(x, fromIntegral x) | x <- [-15000 .. 15000]]

instance Arbitrary ProgramState where
  arbitrary = ProgramState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Stmt where
  arbitrary =
    frequency
      [ (5, elements [Increment, Decrement, MoveRight, MoveLeft, CharIn, CharOut]),
        (1, Loop <$> scale (`div` 3) (listOf1 arbitrary))
      ]

programStateWithNonEmptyProgram :: Gen ProgramState
programStateWithNonEmptyProgram = ProgramState <$> arbitrary <*> arbitrary <*> listOf1 (arbitrary :: Gen Stmt) <*> arbitrary

programStateWithTape :: Tape -> Gen ProgramState
programStateWithTape tape = ProgramState tape <$> arbitrary <*> arbitrary <*> arbitrary

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