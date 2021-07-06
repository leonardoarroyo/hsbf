module Main where

import Control.Monad.State

data Stmt = Plus | Minus | GT | LT | In | Out | LoopStart | LoopEnd

data Prog
  = Seq [Stmt]
  | Loop Prog

data ProgramState = ProgramState
  { tape :: [Int],
    ptr :: Int
  }

--main = print $ evalState (playGame "abcaaacbbcabbab") startState
