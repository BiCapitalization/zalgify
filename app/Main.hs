module Main where

import OptionParser
import ZalgoGenerator
import Control.Monad.State.Lazy
import System.Random

main :: IO ()
main = parseOptions >>= toZalgo

toZalgo :: Options -> IO ()
toZalgo (Options outMode inMode) = toOut outMode =<< zalg <$> fromIn inMode
    where
        zalg :: String -> String
        zalg text = evalState (zalgify (Parameters 8) text) 
            (mkStdGen 8)

        fromIn :: InputMode -> IO String
        fromIn (FromFile path) = readFile path
        fromIn (Direct text)   = return text
        
        toOut :: Maybe String -> String -> IO ()
        toOut (Just file) = writeFile file 
        toOut _           = putStrLn
