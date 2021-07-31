module Cli where

import Options.Applicative

data Options = Options
  { version :: Bool,
    files :: [String]
  }

input :: Parser Options
input =
  Options
    <$> switch
      ( long "version"
          <> short 'v'
          <> help "Print interpreter version"
      )
    <*> many (argument str (metavar "FILES"))

opts :: ParserInfo Options
opts =
  info
    (input <**> helper)
    ( fullDesc
        <> progDesc "Interpret brainfuck source code for FILES"
        <> header "hsbf - a brainfuck interpreter written in Haskell"
    )

cli :: (Options -> IO a) -> IO a
cli fn = fn =<< execParser opts