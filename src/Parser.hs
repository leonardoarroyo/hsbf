{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Ast
import Control.Monad.Identity (Identity)
import Data.Function (on)
import Text.Parsec

rPlus :: Parsec String () Stmt
rPlus = char '+' >> return Increment

rMinus :: Parsec String () Stmt
rMinus = char '-' >> return Decrement

rGT :: Parsec String () Stmt
rGT = char '>' >> return MoveRight

rLT :: Parsec String () Stmt
rLT = char '<' >> return MoveLeft

rComma :: Parsec String () Stmt
rComma = char ',' >> return CharIn

rDot :: Parsec String () Stmt
rDot = char '.' >> return CharOut

rLoop :: ParsecT String () Identity Stmt
rLoop = between (char '[') (char ']') (Text.Parsec.many rTermInstruction) >>= \stmts -> return $ Loop stmts

rTermInstruction :: ParsecT String () Identity Stmt
rTermInstruction = choice [rPlus, rMinus, rGT, rLT, rComma, rDot, rLoop]

parseStmtSeq :: Parsec String () [Stmt]
parseStmtSeq = Text.Parsec.many rTermInstruction