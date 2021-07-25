-- Use efficient data structure for tape
-- Improve main loop
-- Refactor / simplify
-- Implement loops
-- Implement debugger
-- Implement CLI
-- Implement compiler
-- Implement tests
-- CI/CD
-- Wrap around 255
{-# LANGUAGE TupleSections #-}

module Main where

--import Data.Sequence

import Ast
import Control.DeepSeq
import Control.Lens (element, (&), (.~))
import Control.Monad
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
import qualified Data.Array as A
import Data.Char
import Data.Either
import qualified Data.Ix as I
import Data.Word
import Debug.Trace
import Parser
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, getStdGen, randomR)
import Text.Parsec (parse)

type Prog = [Stmt]

type ProgramValue = Int

type Tape = [Word8]

data Status = Running | Exited deriving (Eq)

data ProgramState = ProgramState
  { tape :: Tape,
    ptr :: Int,
    prog :: Prog,
    prog_stack :: [Prog]
  }
  deriving (Show, Eq)

type ProgramStateM = State ProgramState

advance (x : xs) = xs

newPState :: [Stmt] -> ProgramState
newPState program =
  ProgramState
    [0 | i <- [0 .. 100]]
    0
    program
    []

increment :: StateT ProgramState IO Status
increment = state f
  where
    f xs = (Running, ProgramState newTape (ptr xs) (prog xs) (prog_stack xs))
      where
        actualTape = tape xs
        pointer = ptr xs
        newTape = actualTape & element pointer .~ ((actualTape !! pointer) + 1)

decrement :: StateT ProgramState IO Status
decrement = state f
  where
    f xs = (Running, ProgramState newTape (ptr xs) (prog xs) (prog_stack xs))
      where
        actualTape = tape xs
        pointer = ptr xs
        newTape = actualTape & element pointer .~ ((actualTape !! pointer) - 1)

moveRight :: StateT ProgramState IO Status
moveRight = state (\xs -> (Running, ProgramState (tape xs) (ptr xs + 1) (prog xs) (prog_stack xs)))

moveLeft :: StateT ProgramState IO Status
moveLeft = state (\xs -> (Running, ProgramState (tape xs) (ptr xs - 1) (prog xs) (prog_stack xs)))

word8ToString :: Word8 -> String
word8ToString x = [chr (read $ show x :: Int)]

charOut :: StateT ProgramState IO Status
charOut = do
  st <- get
  io $ putStr $ word8ToString $ tape st !! ptr st
  state (Running,)

charIn :: StateT ProgramState IO Status
charIn = do
  char <- io getChar
  state $ f char
  where
    f char s = (Running, ProgramState newTape (ptr s) (prog s) (prog_stack s))
      where
        actualTape = tape s
        pointer = ptr s
        newTape = actualTape & element pointer .~ fromIntegral (ord char)

popInstruction :: StateT ProgramState IO Stmt
popInstruction = do
  st' <- get
  state f
  where
    f s = (headStatement prog', ProgramState (tape s) (ptr s) newProg (prog_stack s))
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
  MoveRight -> moveRight
  MoveLeft -> moveLeft
  Increment -> increment
  Decrement -> decrement
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

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  case parse parseStmtSeq "unknown" prog of
    Right stmts -> void (runStateT runTestPrg (newPState stmts))
    Left err -> print "a"
  where
    prog = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.+++++++..+++.>.<<-.>.+++.------.--------.>+.>++."

io :: IO a -> StateT ProgramState IO a
io = liftIO