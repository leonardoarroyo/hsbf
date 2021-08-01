{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInterpreter where

import Ast
import Control.Arrow
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

instance Arbitrary ProgramState where
  arbitrary = ProgramState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Stmt where
  arbitrary =
    frequency
      [ (5, elements [Increment, Decrement, MoveRight, MoveLeft, CharIn, CharOut]),
        (1, Loop <$> scale (`div` 3) (listOf1 arbitrary))
      ]

interpreterTests :: TestTree
interpreterTests =
  testGroup
    "Interpreter"
    [ testGroup
        "newProgramiterateWhile"
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
            $ \x -> getTapeCell mirroredTape x == Just (fromIntegral x),
          testProperty
            "getTapeCell tape ptr returns Nothing when index ptr is not defined"
            $ \x -> isNothing (getTapeCell newTape x)
        ],
      testGroup
        "putTapeCell"
        [ testProperty "getTapeCell (putTapeCell tape ptr value) ptr == Just value" $
            forAll (arbitrary :: Gen (Int, Word8)) $
              \(ptr, val) -> getTapeCell (putTapeCell mirroredTape ptr val) ptr == Just val
        ],
      testGroup
        "getCell"
        [ testProperty "getCell s == 0 when index at ptr is undefined" $
            forAll (programStateWithTape newTape) $
              \s -> getCell s == 0,
          testProperty "(getCell s) returns element at index (ptr s) on (tape s) when index at ptr is defined" $
            forAll (programStateWithTape mirroredTape) $
              \s -> Just (getCell s) == M.lookup (ptr s) (tape s)
        ],
      testGroup
        "consumeProg"
        [ testProperty "consumeProg s drops (prog s) head" $
            \s -> consumeProg s == s {prog = tailSafe (prog s)}
        ],
      testGroup
        "updatePtr"
        [ testProperty "(updatePtr fn s) applies fn to cell at (ptr s) and return an updated ProgramState" $
            forAll (arbitrary :: Gen (Int -> Int, ProgramState)) $
              \(fn, s) -> updatePtr fn s == s {ptr = fn (ptr s)}
        ],
      testGroup
        "continue"
        [ testProperty "continue s == (Running, s)" $
            \s -> continue s == (Running, s)
        ],
      testGroup
        "loadProgram"
        [ testProperty "(loadProgram stmts s) sets prog field on s to stmts" $
            \(stmts, s) -> loadProgram stmts s == s {prog = stmts}
        ],
      testGroup
        "cellIsZero"
        [ testProperty "(cellIsZero s) == True when cell at ptr is 0" $
            forAll (programStateWithTape newTape) $ \s -> cellIsZero s
            --testProperty "(cellIsZero s) == False when cell at ptr is not 0" $
            --  forAll (programStateWithTape newTape) $ \s -> cellIsZero s
        ]
    ]
  where
    programStateWithTape tape = ProgramState tape <$> arbitrary <*> arbitrary <*> arbitrary
    mirroredTape :: Tape = M.fromList [(x, fromIntegral x) | x <- [-15000 .. 15000]]

--genNonZeroTape = M.fromList [(x, arbitrary :: Gen Word8) | x <- [-15000 .. 15000]]