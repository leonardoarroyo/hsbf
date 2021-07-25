{-# LANGUAGE TupleSections #-}

module Interpreter where

import Ast
import Control.Lens (element, (&), (.~))
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
import qualified Data.Array as A
import Data.Char
import qualified Data.Ix as I
import Data.Word
import Lib (showByte)
import System.Random (StdGen, getStdGen, randomR)
import Text.Parsec (parse)

data Status = Running | Exited deriving (Eq)

data ProgramState = ProgramState
  { tape :: [Word8],
    ptr :: Int,
    prog :: [Stmt],
    prog_stack :: [[Stmt]]
  }
  deriving (Show, Eq)

type ProgramStateM = State ProgramState

newProgram :: [Stmt] -> ProgramState
newProgram program =
  ProgramState
    [0 | i <- [0 .. 100]]
    0
    program
    []

getCell :: ProgramState -> Word8
getCell st = tape st !! ptr st

continue :: b -> (Status, b)
continue x = (Running, x)

putTapeCell :: [Word8] -> Int -> Word8 -> [Word8]
putTapeCell tape ptr value = tape & element ptr .~ value

updateCell :: (Word8 -> Word8) -> ProgramState -> ProgramState
updateCell fn s = s {tape = putTapeCell (tape s) (ptr s) (fn (getCell s))}

updatePtr :: (Int -> Int) -> ProgramState -> ProgramState
updatePtr fn s = s {ptr = fn (ptr s)}

consumeProg :: ProgramState -> ProgramState
consumeProg s = s {prog = tail (prog s)}

headStatement :: [Stmt] -> Stmt
headStatement (x : xs) = x
headStatement [] = Exit

loadProgram :: ProgramState -> [Stmt] -> ProgramState
loadProgram st stmts = st {prog = stmts}

stackProgram :: ProgramState -> ProgramState
stackProgram st = st {prog = [], prog_stack = prog st : prog_stack st}

restoreLoop :: ProgramState -> [Stmt] -> ProgramState
restoreLoop st stmts = st {prog = Loop stmts : prog st}

popStack :: ProgramState -> ProgramState
popStack st = st {prog = stack_head (prog_stack st), prog_stack = stack_tail (prog_stack st)}
  where
    stack_head (x : xs) = x
    stack_head [] = []
    stack_tail = drop 1

------------------

modifyCell :: (Word8 -> Word8) -> StateT ProgramState IO Status
modifyCell fn = state $ continue . updateCell fn

movePtr :: (Int -> Int) -> StateT ProgramState IO Status
movePtr fn = state $ continue . updatePtr fn

charOut :: StateT ProgramState IO Status
charOut = do
  st <- get
  liftIO $ putStr $ showByte $ getCell st
  state (Running,)

charIn :: StateT ProgramState IO Status
charIn = do
  char <- liftIO getChar
  modifyCell $ const $ fromIntegral $ ord char

popInstruction :: StateT ProgramState IO Stmt
popInstruction = state $ \s -> (headStatement (prog s), consumeProg s)

exit :: StateT ProgramState IO Status
exit = state (Exited,)

loop :: [Stmt] -> StateT ProgramState IO Status
loop stmts = do
  modify stackProgram
  st <- get
  put $ loadProgram st stmts
  iterateWhile (== Running) popAndRun
  modify popStack
  st <- get
  if tape st !! ptr st == 0
    then put st
    else put $ restoreLoop st stmts
  return Running

executeStatement :: Stmt -> StateT ProgramState IO Status
executeStatement stmt = case stmt of
  MoveRight -> movePtr (+ 1)
  MoveLeft -> movePtr $ flip (-) 1
  Increment -> modifyCell (+ 1)
  Decrement -> modifyCell $ flip (-) 1
  CharIn -> charIn
  CharOut -> charOut
  Loop stmts -> loop stmts
  Exit -> exit

popAndRun :: StateT ProgramState IO Status
popAndRun = popInstruction >>= executeStatement

runTestPrg :: StateT ProgramState IO ()
runTestPrg = do
  iterateWhile (== Running) popAndRun
  return ()