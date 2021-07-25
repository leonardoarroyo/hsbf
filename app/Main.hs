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
    Right stmts -> void (runStateT runTestPrg (newPState stmts))
    Left err -> print "a"
  where
    prog = ",+[-.,+]"