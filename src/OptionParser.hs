module OptionParser 
    (InputMode (FromFile, Direct),
    Options (Options),
    output,
    input,
    parseOptions) 
where

import Options.Applicative
import Data.Semigroup ((<>))

data InputMode = FromFile FilePath
    | Direct String

data Options = Options {output :: Maybe String, input :: InputMode}

fileP :: Parser InputMode
fileP = FromFile
    <$> strOption
    (long "file" 
    <> short 'f'
    <> metavar "FILE"
    <> help "Take input from FILE")

textP :: Parser InputMode
textP = Direct 
    <$> argument str
    (metavar "TEXT")

outputP :: Parser (Maybe String)
outputP = optional
    $ strOption
    $ short 'o'
    <> metavar "OUT"
    <> help "Send output to OUT"

parser :: Parser Options
parser = Options <$> outputP <*> (fileP <|> textP) 

parseOptions :: IO Options
parseOptions = execParser opts
    where
        opts = info (parser <**> helper) 
            $ fullDesc 
            <> progDesc "Turn some text into Zalgo"
            <> header "Zalgify"
