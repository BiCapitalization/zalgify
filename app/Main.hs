module Main where

import OptionParser
import ZalgoGenerator
import Control.Monad.State.Lazy
import System.Random

main :: IO ()
main = parseOptions >>= putStrLn . greet

greet :: Options -> String
greet (FromFile path) = "Not yet supported :("
greet (Direct text) = evalState (zalgify (Parameters 8) text) (mkStdGen 8)
