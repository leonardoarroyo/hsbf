-- handle spaces/comment
-- Implement CLI
-- Implement tests
-- CI/CD
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

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  case parseProgram "unknown" prog of
    Right stmts -> void (runStateT run (newProgram stmts))
    Left err -> print err
  where
    prog = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.+++++++..+++.>.<<-.>.+++.------.--------.>+.>++."

--prog = "[,>]"

--prog = ",+[-.,+]"