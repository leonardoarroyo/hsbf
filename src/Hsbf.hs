{-# LANGUAGE TypeApplications #-}

module Hsbf where

import Ast (Stmt)
import Cli (Options (Options))
import Control.Exception (IOException, try)
import Control.Lens (element, (&), (.~))
import Control.Monad (void, (>=>))
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State (StateT (runStateT))
import Data.Functor ((<&>))
import Data.Version (showVersion)
import Interpreter (newProgram, run)
import Parser (parseProgram)
import qualified Paths_hsbf as P
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO
  ( BufferMode (NoBuffering),
    hPrint,
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )
import System.Path (fromFilePath, makeAbsolute, toFilePath)

type GenericError = String

failWithGeneric :: Show a => Either a b -> Either GenericError b
failWithGeneric = either (Left . show) Right

readSourceFile :: FilePath -> IO (Either GenericError String)
readSourceFile path = failWithGeneric <$> (try @IOException $ readFile path)

runFile :: FilePath -> IO ()
runFile path = do
  source <- readSourceFile path
  either fatal void $ (source >>= parse) <&> runStmts
  where
    fatal err = hPutStrLn stderr err >> exitWith (ExitFailure 1)
    runStmts stmts = void (runStateT run (newProgram stmts))
    parse program = failWithGeneric $ parseProgram path program

hsbf :: Options -> IO ()
hsbf (Options True _) = putStrLn $ showVersion P.version
hsbf (Options _ files) = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  runFiles files
  where
    runFiles = mapM_ $ absolutePath >=> runFile
    absolutePath path = toFilePath <$> makeAbsolute (fromFilePath path)