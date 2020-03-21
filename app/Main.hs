module Main where

import OptionParser
import ZalgoGenerator
import Control.Monad.State.Lazy
import System.Random

main :: IO ()
main = parseOptions >>= toZalgo >>= putStrLn

toZalgo :: Options -> IO String
toZalgo s = case s of
    FromFile path -> readFile path >>= zalg
    Direct text   -> zalg text
    where
        zalg text = return $ evalState 
            (zalgify (Parameters 8) text) 
            (mkStdGen 8)
