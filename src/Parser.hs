{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Ast
import Control.Monad.Identity (Identity)
import Data.Function (on)
import Text.Parsec (Parsec, ParsecT, between, char, choice, many)

terminal :: ParsecT String () Identity Stmt
terminal = choice [plus, minus, gt, lt, comma, dot, loop]
  where
    charStmt c v = char c >> return v
    plus = charStmt '+' Increment
    minus = charStmt '-' Decrement
    gt = charStmt '>' MoveRight
    lt = charStmt '<' MoveLeft
    comma = charStmt ',' CharIn
    dot = charStmt '.' CharOut
    loop = between (char '[') (char ']') (many terminal) >>= \stmts -> return $ Loop stmts

parseStmtSeq :: Parsec String () [Stmt]
parseStmtSeq = many terminal