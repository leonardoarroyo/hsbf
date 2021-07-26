{-# LANGUAGE TupleSections #-}

module Interpreter where

import Ast (Stmt (..), Stmt2 (..))
import Control.Lens (element, (&), (.~))
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, state),
    State,
    StateT,
    modify,
  )
import Data.Char (ord)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Lib (showByte)
import System.Random (StdGen, getStdGen, randomR)
import Text.Parsec (parse)

data Status = Running | Exited deriving (Eq)

--type Tape = [Word8]
type Tape = M.Map Int Word8

data ProgramState = ProgramState
  { tape :: Tape,
    ptr :: Int,
    prog :: [Stmt],
    prog_stack :: [[Stmt]]
  }
  deriving (Show, Eq)

type ProgramStateM = State ProgramState

--newTape :: Tape
--newTape = replicate 100 0
newTape :: M.Map Int Word8
newTape = M.empty

newProgram :: [Stmt] -> ProgramState
newProgram program = ProgramState newTape 0 program []

--putTapeCell :: Tape -> Int -> Word8 -> Tape
--putTapeCell tape ptr value = tape & element ptr .~ value

putTapeCell :: Tape -> Int -> Word8 -> Tape
putTapeCell tape ptr value = M.insert ptr value tape

--getCell :: ProgramState -> Word8
--getCell st = tape st !! ptr st

getCell :: ProgramState -> Word8
getCell st = fromMaybe 0 (M.lookup (ptr st) (tape st))

cellIsZero :: ProgramState -> Bool
cellIsZero = (==) 0 . getCell

continue :: ProgramState -> (Status, ProgramState)
continue = (Running,)

updateCell :: (Word8 -> Word8) -> ProgramState -> ProgramState
updateCell fn s = s {tape = putTapeCell (tape s) (ptr s) (fn (getCell s))}

updatePtr :: (Int -> Int) -> ProgramState -> ProgramState
updatePtr fn s = s {ptr = fn (ptr s)}

consumeProg :: ProgramState -> ProgramState
consumeProg s = s {prog = tail (prog s)}

headStatement :: [Stmt] -> Stmt
headStatement (x : xs) = x
headStatement [] = Exit

loadProgram :: [Stmt] -> ProgramState -> ProgramState
loadProgram stmts st = st {prog = stmts}

stackProgram :: ProgramState -> ProgramState
stackProgram st = st {prog = [], prog_stack = prog st : prog_stack st}

restoreLoop :: [Stmt] -> ProgramState -> ProgramState
restoreLoop stmts st = st {prog = Loop stmts : prog st}

popStack :: ProgramState -> ProgramState
popStack st = st {prog = head' (prog_stack st), prog_stack = drop 1 (prog_stack st)}
  where
    head' (x : xs) = x
    head' [] = []

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

restoreLoopIfNotZero :: [Stmt] -> ProgramState -> ProgramState
restoreLoopIfNotZero stmts st
  | cellIsZero st = st
  | otherwise = restoreLoop stmts st

loop :: [Stmt] -> StateT ProgramState IO ()
loop stmts = do
  start
  iterateWhile (== Running) popAndRun
  end
  where
    start = modify $ loadProgram stmts . stackProgram
    end = modify $ restoreLoopIfNotZero stmts . popStack

exit :: StateT ProgramState IO Status
exit = state (Exited,)

executeStatement :: Stmt -> StateT ProgramState IO Status
executeStatement stmt = case stmt of
  MoveRight -> movePtr (+ 1)
  MoveLeft -> movePtr $ flip (-) 1
  Increment -> modifyCell (+ 1)
  Decrement -> modifyCell $ flip (-) 1
  CharIn -> charIn
  CharOut -> charOut
  Loop stmts -> loop stmts >> return Running
  Exit -> exit

popAndRun :: StateT ProgramState IO Status
popAndRun = popInstruction >>= executeStatement

runProgram :: StateT ProgramState IO ()
runProgram = do
  iterateWhile (== Running) popAndRun
  return ()