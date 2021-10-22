module CommandLine where

import Options.Applicative

data Options = Options
  { input :: FilePath,
    output :: FilePath
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> header "Lua"
        <> progDesc "A Lua 5.1.5 compiler implementation written in Haskell."
    )

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseInput
    <*> parseOutput

parseInput :: Parser String
parseInput =
  strArgument $
    metavar "INPUT_FILE"

parseOutput :: Parser String
parseOutput =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "OUTPUT_FILE"
        <> value "luac.out"
    )
