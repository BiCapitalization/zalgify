module OptionParser 
    (InputMode (FromFile, Direct),
    Options (Options),
    output,
    input,
    level,
    seed,
    parseOptions) 
where

import Options.Applicative
import Data.Semigroup ((<>))

data InputMode = FromFile FilePath
    | Direct String

data Options = Options {
    output :: Maybe String,
    input  :: InputMode,
    level  :: Int,
    seed   :: Maybe Int
}

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

levelP :: Parser Int
levelP = option auto
    $ long "level"
    <> short 'l'
    <> value 5
    <> showDefault
    <> metavar "INT"
    <> help "Maximum number of diacritics to add to each single character"

seedP :: Parser (Maybe Int)
seedP = optional $ option auto
    $ long "seed"
    <> short 's'
    <> metavar "SEED"
    <> help "Value to seed the random generator with"

parser :: Parser Options
parser = Options
    <$> outputP
    <*> (fileP
    <|> textP)
    <*> levelP
    <*> seedP

parseOptions :: IO Options
parseOptions = execParser opts
    where
        opts = info (parser <**> helper) 
            $ fullDesc 
            <> progDesc "Turn some text into Zalgo"
            <> header "Zalgify"
