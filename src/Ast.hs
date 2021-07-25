module Ast where

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