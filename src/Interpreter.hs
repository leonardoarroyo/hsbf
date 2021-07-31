{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Interpreter where

import Ast (Stmt (..))
import Control.Conditional (if', select)
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

type Prog = [Stmt]

data ProgramState = ProgramState
  { tape :: Tape,
    ptr :: Int,
    prog :: Prog,
    progStack :: [Prog]
  }
  deriving (Show, Eq)

type ProgramStateT = StateT ProgramState IO

showByte :: Word8 -> String
showByte x = [chr (read $ show x :: Int)]

newTape :: M.Map Int Word8
newTape = M.empty

newProgram :: Prog -> ProgramState
newProgram program = ProgramState newTape 0 program []

putTapeCell :: Tape -> Int -> Word8 -> Tape
putTapeCell tape ptr value = M.insert ptr value tape

getCell :: ProgramState -> Word8
getCell s = fromMaybe 0 (M.lookup (ptr s) (tape s))

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

headStatement :: Prog -> Stmt
headStatement (x : xs) = x
headStatement [] = Exit

loadProgram :: Prog -> ProgramState -> ProgramState
loadProgram stmts s = s {prog = stmts}

stackProgram :: ProgramState -> ProgramState
stackProgram s = s {prog = [], progStack = prog s : progStack s}

restoreLoop :: Prog -> ProgramState -> ProgramState
restoreLoop stmts s = s {prog = Loop stmts : prog s}

popStack :: ProgramState -> ProgramState
popStack s = s {prog = head' (progStack s), progStack = drop 1 (progStack s)}
  where
    head' (x : xs) = x
    head' [] = []

--------------

modifyCell :: (Word8 -> Word8) -> ProgramStateT Status
modifyCell fn = state $ continue . updateCell fn

movePtr :: (Int -> Int) -> ProgramStateT Status
movePtr fn = state $ continue . updatePtr fn

charOut :: ProgramStateT Status
charOut = get >>= (liftIO . putStr . showByte . getCell) >> return Running

charIn :: ProgramStateT Status
charIn = liftIO getChar >>= modifyCell . const . fromIntegral . ord

popStmt :: ProgramStateT Stmt
popStmt = state $ \s -> (headStatement (prog s), consumeProg s)

loop :: Prog -> ProgramStateT Status
loop stmts = start >> eval >> end
  where
    onlyIfNotZero = select cellIsZero id
    runIfNotZero s = if' (cellIsZero s) (return Running) run
    start = modify $ onlyIfNotZero (loadProgram stmts) . stackProgram
    eval = get >>= runIfNotZero
    end = state $ continue . onlyIfNotZero (restoreLoop stmts) . popStack

exit :: ProgramStateT Status
exit = state (Exited,)

executeStmt :: Stmt -> ProgramStateT Status
executeStmt = \case
  MoveRight -> movePtr (+ 1)
  MoveLeft -> movePtr $ flip (-) 1
  Increment -> modifyCell (+ 1)
  Decrement -> modifyCell $ flip (-) 1
  CharIn -> charIn
  CharOut -> charOut
  Loop stmts -> loop stmts
  Exit -> exit
  Noop -> return Running

popAndRun :: ProgramStateT Status
popAndRun = popStmt >>= executeStmt

run :: ProgramStateT Status
run = iterateWhile (== Running) popAndRun