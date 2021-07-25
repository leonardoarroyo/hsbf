-- Infinite tape
-- CI/CD
-- handle spaces/comment
-- fix loop initial condition
-- Implement tests
-- Implement CLI
-- Implement debugger
-- Implement compiler
{-# LANGUAGE TupleSections #-}

module Main where

import Ast
import Control.Lens (element, (&), (.~))
import Control.Monad
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
import Interpreter
import Parser
import System.IO
import Text.Parsec (parse)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  case parse parseStmtSeq "unknown" prog of
    Right stmts -> void (runStateT runProgram (newProgram stmts))
    Left err -> print "a"
  where
    --prog = ",+[-.,+]"
    prog = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.+++++++..+++.>.<<-.>.+++.------.--------.>+.>++."