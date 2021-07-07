module Main where

import Control.Lens (element, (&), (.~))
import Control.Monad.State
import Data.Sequence
import Debug.Trace

data Stmt = Increment | Decrement | MoveRight | MoveLeft deriving (Show)

type Prog = Seq Stmt

type ProgramValue = Int

type Tape = [Int]

data ProgramState = ProgramState
  { tape :: Tape,
    ptr :: Int
  }
  deriving (Show)

type ProgramStateM = State ProgramState

newState :: Int -> ProgramState
newState memorySize = ProgramState [0 | i <- [0 .. memorySize -1]] 0

processStatement :: Stmt -> ProgramState -> ProgramState
processStatement stmt state =
  case stmt of
    Increment -> ProgramState (tape' & element ptr' .~ ((tape' !! ptr') + 1)) ptr'
    Decrement -> ProgramState (tape' & element ptr' .~ ((tape' !! ptr') - 1)) ptr'
    MoveRight -> ProgramState tape' (ptr' + 1)
    MoveLeft -> ProgramState tape' (ptr' - 1)
  where
    tape' = tape state
    ptr' = ptr state

runStatement :: Stmt -> ProgramStateM ()
runStatement stmt = do
  modify (processStatement stmt)

evalProgram :: Prog -> ProgramStateM ProgramState
evalProgram prog | trace ("evalProgram " ++ show prog) False = undefined
evalProgram prog = do
  runStatement stmt
  if hasMore then evalProgram tail else get
  get
  where
    stmt = Data.Sequence.index prog 0
    tail = Data.Sequence.drop 1 prog
    hasMore = Data.Sequence.length tail > 0

main :: IO ()
main =
  do
    print $ execState (evalProgram basicProgram) zero
  where
    basicProgram = fromList [Decrement, MoveRight, Increment, MoveRight, MoveRight, Increment, Increment]
    zero = newState 1000