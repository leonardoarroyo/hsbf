{-# LANGUAGE TupleSections #-}

module Interpreter where

import Ast (Stmt (..))
import Control.Lens (element, (&), (.~))
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, state),
    State,
    StateT,
    modify,
    put,
    void,
  )
import Data.Char (chr, ord)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import System.Random (StdGen, getStdGen, randomR)
import Text.Parsec (parse)

data Status = Running | Exited deriving (Eq)

type Tape = M.Map Int Word8

data ProgramState = ProgramState
  { tape :: Tape,
    ptr :: Int,
    prog :: [Stmt],
    prog_stack :: [[Stmt]]
  }
  deriving (Show, Eq)

type ProgramStateT = StateT ProgramState IO

showByte :: Word8 -> String
showByte x = [chr (read $ show x :: Int)]

newTape :: M.Map Int Word8
newTape = M.empty

newProgram :: [Stmt] -> ProgramState
newProgram program = ProgramState newTape 0 program []

putTapeCell :: Tape -> Int -> Word8 -> Tape
putTapeCell tape ptr value = M.insert ptr value tape

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

--------------

modifyCell :: (Word8 -> Word8) -> ProgramStateT Status
modifyCell fn = state $ continue . updateCell fn

movePtr :: (Int -> Int) -> ProgramStateT Status
movePtr fn = state $ continue . updatePtr fn

charOut :: ProgramStateT Status
charOut = get >>= (liftIO . putStr . showByte . getCell) >> state (Running,)

charIn :: ProgramStateT Status
charIn = liftIO getChar >>= modifyCell . const . fromIntegral . ord

popInstruction :: ProgramStateT Stmt
popInstruction = state $ \s -> (headStatement (prog s), consumeProg s)

ifNotZero :: (ProgramState -> ProgramState) -> ProgramState -> ProgramState
ifNotZero fn st
  | cellIsZero st = st
  | otherwise = fn st

runIfNotZero :: ProgramState -> ProgramStateT Status
runIfNotZero st
  | cellIsZero st = return Running
  | otherwise = run

loop :: [Stmt] -> ProgramStateT ()
loop stmts = start >> run >> end
  where
    start = modify $ ifNotZero (loadProgram stmts) . stackProgram
    run = get >>= runIfNotZero
    end = modify $ ifNotZero (restoreLoop stmts) . popStack

exit :: ProgramStateT Status
exit = state (Exited,)

executeStatement :: Stmt -> ProgramStateT Status
executeStatement stmt = case stmt of
  MoveRight -> movePtr (+ 1)
  MoveLeft -> movePtr $ flip (-) 1
  Increment -> modifyCell (+ 1)
  Decrement -> modifyCell $ flip (-) 1
  CharIn -> charIn
  CharOut -> charOut
  Loop stmts -> loop stmts >> return Running
  Exit -> exit

popAndRun :: ProgramStateT Status
popAndRun = popInstruction >>= executeStatement

run :: ProgramStateT Status
run = iterateWhile (== Running) popAndRun