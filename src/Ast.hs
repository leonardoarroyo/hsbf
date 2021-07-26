module Ast where

import GHC.Generics

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

data Stmt2
  = Increment2
  | Decrement2
  | Exit2
  deriving (Show, Eq)