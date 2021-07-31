{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Ast
import Control.Monad.Identity (Identity)
import Data.Function (on)
import Text.Parsec
  ( ParseError,
    Parsec,
    ParsecT,
    SourceName,
    between,
    char,
    choice,
    many,
    noneOf,
    parse,
  )

terminal :: ParsecT String () Identity Stmt
terminal = choice [plus, minus, gt, lt, comma, dot, noop, loop]
  where
    charStmt c v = char c >> return v
    plus = charStmt '+' Increment
    minus = charStmt '-' Decrement
    gt = charStmt '>' MoveRight
    lt = charStmt '<' MoveLeft
    comma = charStmt ',' CharIn
    dot = charStmt '.' CharOut
    noop = noneOf "+-><,.[]" >> return Noop
    loop = Loop <$> between (char '[') (char ']') (many terminal)

parseStmtSeq :: Parsec String () [Stmt]
parseStmtSeq = many terminal

parseProgram :: SourceName -> String -> Either ParseError [Stmt]
parseProgram = parse parseStmtSeq