module CommandLine where

import Options.Applicative

newtype Options = Options {input :: FilePath}

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

parseInput :: Parser String
parseInput =
  strArgument $
    metavar "INPUT_FILE"
