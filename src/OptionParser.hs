module OptionParser 
    (Options (FromFile, Direct),
    parseOptions) 
where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = FromFile FilePath
    | Direct String

file :: Parser Options
file = FromFile
    <$> strOption
    (long "file" 
    <> short 'f'
    <> metavar "FILE"
    <> help "Take input from FILE")

text :: Parser Options
text = Direct 
    <$> argument str
    (metavar "TEXT")

parser :: Parser Options
parser = file <|> text

parseOptions :: IO Options
parseOptions = execParser opts
    where
        opts = info (parser <**> helper) 
            $ fullDesc 
            <> progDesc "Turn some text into Zalgo"
            <> header "Zalgify"
