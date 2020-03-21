module Main where

import OptionParser
import ZalgoGenerator
import Control.Monad.State.Lazy
import System.Random

main :: IO ()
main = parseOptions >>= toZalgo

toZalgo :: Options -> IO ()
toZalgo (Options outMode inMode lvl seed) = toOut outMode
    =<< zalg <$> rng <*> fromIn inMode
    where
        rng :: IO StdGen
        rng = case seed of
            Just value -> return $ mkStdGen value
            _          -> getStdGen

        zalg :: RandomGen g => g -> String -> String
        zalg gen text = evalState (zalgify (Parameters lvl) text) gen

        fromIn :: InputMode -> IO String
        fromIn (FromFile path) = readFile path
        fromIn (Direct text)   = return text
        
        toOut :: Maybe String -> String -> IO ()
        toOut (Just file) = writeFile file 
        toOut _           = putStrLn
