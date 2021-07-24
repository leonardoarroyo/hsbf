-- TODO:
-- Use efficient data structure for tape
-- Improve main loop
-- Refactor / simplify
-- Implement loops
-- Implement debugger
-- Implement CLI
-- Implement compiler
-- Implement tests
module Main where

import Control.DeepSeq
import Control.Lens (element, (&), (.~))
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.Array as A
--import Data.Sequence

import Data.Char
import qualified Data.Ix as I
import Debug.Trace
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, getStdGen, randomR)

data Stmt
  = Increment
  | Decrement
  | MoveRight
  | MoveLeft
  | CharIn
  | CharOut
  | Exit
  deriving (Show, Eq)

type Prog = [Stmt]

type ProgramValue = Int

type Tape = [Int]

data Status = Running | Exited deriving (Eq)

data ProgramState = ProgramState
  { tape :: Tape,
    ptr :: Int,
    prog :: Prog
  }
  deriving (Show, Eq)

type ProgramStateM = State ProgramState

advance (x : xs) = xs

newPState :: ProgramState
newPState =
  ProgramState
    [100 | i <- [0 .. 100]]
    0
    [CharIn, MoveRight, CharIn, MoveLeft, CharOut, MoveRight, CharOut]

increment :: StateT ProgramState IO Status
increment = state f
  where
    f xs = (Running, ProgramState newTape (ptr xs) (prog xs))
      where
        actualTape = tape xs
        pointer = ptr xs
        newTape = actualTape & element pointer .~ ((actualTape !! pointer) + 1)

decrement :: StateT ProgramState IO Status
decrement = state f
  where
    f xs = (Running, ProgramState newTape (ptr xs) (prog xs))
      where
        actualTape = tape xs
        pointer = ptr xs
        newTape = actualTape & element pointer .~ ((actualTape !! pointer) - 1)

moveRight :: StateT ProgramState IO Status
moveRight = state (\xs -> (Running, ProgramState (tape xs) (ptr xs + 1) (prog xs)))

moveLeft :: StateT ProgramState IO Status
moveLeft = state (\xs -> (Running, ProgramState (tape xs) (ptr xs - 1) (prog xs)))

charOut :: StateT ProgramState IO Status
charOut = do
  st <- get
  io $ putStr [chr (tape st !! ptr st)]
  state (\xs -> (Running, xs))

charIn :: StateT ProgramState IO Status
charIn = do
  char <- io getChar
  state $ f char
  where
    f char s = (Running, ProgramState newTape (ptr s) (prog s))
      where
        actualTape = tape s
        pointer = ptr s
        newTape = actualTape & element pointer .~ ord char

popInstruction :: StateT ProgramState IO Stmt
popInstruction = do
  st' <- get
  state f
  where
    f s = (headStatement prog', ProgramState (tape s) (ptr s) newProg)
      where
        prog' = prog s
        headStatement (x : xs) = x
        headStatement [] = Exit
        newProg = drop 1 prog'

peekInstruction :: StateT ProgramState IO Stmt
peekInstruction = do
  st' <- get
  state f
  where
    f s = (headStatement prog', s)
      where
        prog' = prog s
        headStatement (x : xs) = x
        headStatement [] = Exit

exit :: StateT ProgramState IO Status
exit = state (\s -> (Exited, s))

executeStatement :: Stmt -> StateT ProgramState IO Status
executeStatement stmt = case stmt of
  MoveRight -> moveRight
  MoveLeft -> moveLeft
  Increment -> increment
  Decrement -> decrement
  CharIn -> charIn
  CharOut -> charOut
  Exit -> exit

popAndRun :: StateT ProgramState IO Status
popAndRun = do
  instruction <- popInstruction
  executeStatement instruction

runTestPrg :: StateT ProgramState IO ()
runTestPrg = do
  iterateWhile (\a -> a == Running) popAndRun
  st <- get
  io $ print st
  return ()

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  void (runStateT runTestPrg newPState)

io :: IO a -> StateT ProgramState IO a
io = liftIO

-- --------------------------
-- --------------------------
-- newState :: Int -> ProgramState
-- newState memorySize = ProgramState [0 | i <- [0 .. memorySize -1]] 0
--
-- processStatement :: Stmt -> ProgramState -> ProgramState
-- processStatement stmt state =
--   case stmt of
--     Increment -> ProgramState (tape' & element ptr' .~ ((tape' !! ptr') + 1)) ptr'
--     Decrement -> ProgramState (tape' & element ptr' .~ ((tape' !! ptr') - 1)) ptr'
--     MoveRight -> ProgramState tape' (ptr' + 1)
--     MoveLeft -> ProgramState tape' (ptr' - 1)
--   where
--     tape' = tape state
--     ptr' = ptr state
--
-- runStatement :: Stmt -> ProgramStateM ()
-- runStatement stmt = do
--   modify (processStatement stmt)
--
-- evalProgram :: Prog -> ProgramStateM ProgramState
-- evalProgram prog | trace ("evalProgram \n" ++ show stmt ++ "|" ++ show tail ++ "\n" ++ show hasMore ++ "\n") False = undefined
--   where
--     stmt = if Data.Sequence.length prog > 0 then Just (Data.Sequence.index prog 0) else Nothing
--     tail = Data.Sequence.drop 1 prog
--     hasMore = Data.Sequence.length tail > 0
-- evalProgram prog =
--   get >>= \st ->
--     case stmt of
--       Just stmt -> case stmt of
--         Loop a -> if tape st !! ptr st == 0 then get else evalProgram a `seq` evalProgram prog
--         _ -> runStatement stmt >> get
--       Nothing -> get
--       `seq` if hasMore then evalProgram tail else get
--   where
--     stmt = if Data.Sequence.length prog > 0 then Just (Data.Sequence.index prog 0) else Nothing
--     tail = Data.Sequence.drop 1 prog
--     hasMore = Data.Sequence.length tail > 0
--
-- strict :: (t -> b) -> t -> b
-- strict f x = seq x (f x)

--main :: IO ()
--main =
--  do
--    print $ execState (evalProgram basicProgram) zero
--  where
--    basicProgram = fromList [Increment, Increment, Increment, Loop loopInner]
--    loopInner = strict fromList [Decrement, MoveRight, Increment, MoveRight, Increment, Increment, Increment, MoveLeft, MoveLeft] --, Loop innerMost]
--    innerMost = strict fromList [Decrement, MoveRight, Increment, MoveLeft]
--    zero = newState 1000

------------------------
--