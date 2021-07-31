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
  | Noop
  deriving (Show, Eq)