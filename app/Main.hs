-- TODO:
-- Use efficient data structure for tape
-- Improve main loop
-- Refactor / simplify
-- Implement loops
-- Implement debugger
-- Implement CLI
-- Implement compiler
-- Implement tests
-- CI/CD
{-# LANGUAGE TupleSections #-}

module Main where

import Control.DeepSeq
import Control.Lens (element, (&), (.~))
import Control.Monad
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
import qualified Data.Array as A
--import Data.Sequence

import Data.Char
import Data.Either
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
  | Loop [Stmt]
  | Exit
  deriving (Show, Eq)

type Prog = [Stmt]

type ProgramValue = Int

type Tape = [Int]

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

newPState :: ProgramState
newPState =
  ProgramState
    [0 | i <- [0 .. 100]]
    0
    [Increment, Increment, Increment, Loop [Decrement, MoveRight, Increment, Increment, Loop [Decrement, MoveRight, Increment, Increment, MoveLeft], MoveLeft]]
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

charOut :: StateT ProgramState IO Status
charOut = do
  st <- get
  io $ putStr [chr (tape st !! ptr st)]
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
        newTape = actualTape & element pointer .~ ord char

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
  st <- get
  io $ print st
  return ()

--main :: IO ()
--main = do
--  hSetBuffering stdin NoBuffering
--  void (runStateT runTestPrg newPState)

io :: IO a -> StateT ProgramState IO a
io = liftIO

----------------------
--
data Token = PLUS | MINUS | GT' | LT' | COMMA | DOT | BRACKET_OPEN | BRACKET_CLOSE deriving (Show)

type TokenList = [Token]

tokenForChar :: Char -> Either ParseError Token
tokenForChar '+' = Right PLUS
tokenForChar '-' = Right MINUS
tokenForChar '>' = Right GT'
tokenForChar '<' = Right LT'
tokenForChar ',' = Right COMMA
tokenForChar '.' = Right DOT
tokenForChar '[' = Right BRACKET_OPEN
tokenForChar ']' = Right BRACKET_CLOSE
tokenForChar x = Left $ ParseError InvalidCharacter ("Could not parse character: " ++ [x])

data ParseErrorCode = InvalidCharacter | MismatchingBrackets deriving (Show)

data ParseError = ParseError
  { code :: ParseErrorCode,
    message :: String
  }
  deriving (Show)

lexer :: String -> Either ParseError TokenList
lexer str =
  case partitionEithers result of
    ([], lst) -> Right lst
    (x : xs, _) -> Left x
  where
    result = [tokenForChar c | c <- str]

data ParseState = ParseState
  { tokenList :: TokenList,
    current :: [Stmt],
    stack :: [[Stmt]]
  }
  deriving (Show)

popToken :: State ParseState Token
popToken = do
  st <- get
  state f
  where
    f s = (headToken tokenList', ParseState (tokenListTail tokenList') (current s) (stack s))
      where
        tokenList' = tokenList s
        headToken (x : xs) = x
        tokenListTail = drop 1

parse :: State ParseState ()
parse = do
  tk <- popToken
  return ()

--case tokenList st of
--  (x:xs) ->
--case x of
--  PLUS -> (++) <$> Right [Increment] <*> parse' xs
--  MINUS -> (++) <$> Right [Decrement] <*> parse' xs
--  GT' -> (++) <$> Right [MoveRight] <*> parse' xs
--  LT' -> (++) <$> Right [MoveLeft] <*> parse' xs
--  COMMA -> (++) <$> Right [CharIn] <*> parse' xs
--  DOT -> (++) <$> Right [CharOut] <*> parse' xs
--  BRACKET_OPEN -> (++) <$> Right [Loop []] <*> parse (ParseState (levelCount st) + 1) xs
--  BRACKET_CLOSE -> Left $ ParseError MismatchingBrackets "Mismatching brackets."
--where
--  parse' = parse st

--main :: IO ()
--main = do
--  let x = lexer "++>[]]+"
--  case x of
--    Right lx -> print $ parse lx (ParseState 0)
--    Left err -> print err
main = do
  let x = lexer "++>[]]+"
  case x of
    Right lx -> print $ runState parse (ParseState lx [] [])
    Left err -> print err