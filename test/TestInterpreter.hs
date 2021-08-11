{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module TestInterpreter where

import Ast
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State (MonadTrans, StateT (StateT, runStateT))
import Data.Bifunctor
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

interpreterUtilsTests :: TestTree
interpreterUtilsTests =
  testGroup
    "Interpreter utils"
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
            $ \x -> getTapeCell mirroredTape x == Just (fromIntegral x),
          testProperty
            "getTapeCell tape ptr returns Nothing when index ptr is not defined"
            $ \x -> isNothing (getTapeCell newTape x)
        ],
      testGroup
        "putTapeCell"
        [ testProperty "getTapeCell (putTapeCell tape ptr value) ptr == Just value" $
            forAll (arbitrary :: Gen (Int, Cell)) $
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
            forAll (programStateWithTape newTape) $ \s -> cellIsZero s,
          testProperty "(cellIsZero s) == False when cell at ptr is not 0" $
            forAll (nonZeroTape >>= programStateWithTape) $ \s -> not (cellIsZero s)
        ],
      testGroup
        "updateCell"
        [ testProperty "(updateCell fn s) applies fn to cell at (ptr s) and updates the value" $
            \(fn, s) -> updateCell fn s == s {tape = M.insert (ptr s) (fn (getCell s)) (tape s)}
        ],
      testGroup
        "stackProgram"
        [ testProperty "(stackProgram s) moves current (prog s) to head of (progStack s) and sets (prog s) to []" $
            \s -> stackProgram s == s {prog = [], progStack = prog s : progStack s}
        ],
      testGroup
        "restoreLoop"
        [ testProperty "(restoreLoop stmts s) puts a (Loop stmts) at head of (prog s)" $
            \(stmts, s) -> restoreLoop stmts s == s {prog = Loop stmts : prog s}
        ],
      testGroup
        "popStack"
        [ testProperty "(popStack s) pops head of (progStack s) and puts it at (prog s) when (progStack s) is not empty" $
            forAll (programStateWithStack (arbitrary `suchThat` (not . null))) $
              \s -> popStack s == s {prog = head (progStack s), progStack = drop 1 (progStack s)},
          testProperty "(popStack s) sets (prog s) to [] when (progStack s) == []" $
            forAll (programStateWithStack (return [])) $
              \s -> popStack s == s {prog = [], progStack = []}
        ]
    ]
  where
    programStateWithTape tape = ProgramState tape <$> arbitrary <*> arbitrary <*> arbitrary
    programStateWithStack stack = ProgramState <$> arbitrary <*> arbitrary <*> arbitrary <*> stack
    mirroredTape :: Tape = M.fromList [(x, fromIntegral x) | x <- [-15000 .. 15000]]
    nonZeroTape :: Gen Tape = M.fromList <$> sequence [nonZero >>= \y -> return (x, y) | x <- [-15000 .. 15000]]
    nonZero :: Gen Cell = arbitrary `suchThat` (/= 0)

interpreterVMTests :: TestTree
interpreterVMTests =
  testGroup
    "Interpreter VM"
    [ testGroup
        "When executing a single statement"
        [ testCase
            "MoveRight should increase ptr"
            $ runStmt MoveRight >>= (snd >>> ptr >>> (@?= 1)),
          testCase
            "MoveLeft should decrease ptr"
            $ runStmt MoveLeft >>= (snd >>> ptr >>> (@?= -1)),
          testCase
            "Increment should increase cell value under ptr"
            $ runStmt Increment >>= (snd >>> getCell >>> (@?= 1)),
          testCase
            "Decrement should decrease cell value under ptr"
            $ runStmt Decrement >>= (snd >>> getCell >>> (@?= ((-) 0 1 :: Cell))),
          testCase
            "Exit should return Exited status"
            $ runStmt Exit >>= (fst >>> (@?= Exited))
        ]
    ]
  where
    runStmt stmt = runStateT run (newProgram [stmt])