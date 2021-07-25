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
import System.IO.Unsafe (unsafePerformIO)
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

modifyCell :: (Word8 -> Word8) -> StateT ProgramState IO Status
modifyCell fn = state f
  where
    f s = (Running, s {tape = newTape})
      where
        tape' = tape s
        ptr' = ptr s
        newTape = tape' & element ptr' .~ fn (getCell s)

movePtr :: (Int -> Int) -> StateT ProgramState IO Status
movePtr f = state (\s -> (Running, s {ptr = f (ptr s)}))

charOut :: StateT ProgramState IO Status
charOut = do
  st <- get
  io $ putStr $ showByte $ getCell st
  state (Running,)

charIn :: StateT ProgramState IO Status
charIn = do
  char <- io getChar
  state $ f char
  where
    f char s = (Running, s {tape = newTape})
      where
        actualTape = tape s
        pointer = ptr s
        newTape = actualTape & element pointer .~ fromIntegral (ord char)

popInstruction :: StateT ProgramState IO Stmt
popInstruction = do
  st' <- get
  state f
  where
    f s = (headStatement prog', s {prog = newProg})
      where
        prog' = prog s
        headStatement (x : xs) = x
        headStatement [] = Exit
        newProg = drop 1 prog'

exit :: StateT ProgramState IO Status
exit = state (Exited,)

loop :: [Stmt] -> StateT ProgramState IO Status
loop stmts = do
  st <- get
  put $ stack_program st
  st <- get
  put $ load_program st stmts
  iterateWhile (== Running) popAndRun
  st <- get
  put $ pop_stack st
  st <- get
  if tape st !! ptr st == 0
    then put st
    else put $ restore_loop st stmts
  return Running
  where
    f s = (Running, ProgramState (tape s) (ptr s) (prog s) (prog_stack s))
    stack_program st = ProgramState (tape st) (ptr st) [] (prog st : prog_stack st)
    load_program st stmts = ProgramState (tape st) (ptr st) stmts (prog_stack st)
    restore_loop st stmts = ProgramState (tape st) (ptr st) (Loop stmts : prog st) (prog_stack st)
    pop_stack st = ProgramState (tape st) (ptr st) (stack_head (prog_stack st)) (stack_tail (prog_stack st))
      where
        stack_head (x : xs) = x
        stack_head [] = []
        stack_tail = drop 1

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

io :: IO a -> StateT ProgramState IO a
io = liftIO