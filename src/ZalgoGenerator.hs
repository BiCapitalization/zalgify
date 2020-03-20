module ZalgoGenerator
    (zalgify,
    Parameters (Parameters))
where

import Control.Monad.State.Lazy
import System.Random
import Data.Foldable (foldMap)
import Control.Monad

import Debug.Trace

data Parameters = Parameters {level :: Int}

-- These are all the diacritc marks we use, grouped by whether they extend above or
-- below the modified character, or stay right on it.
goingUp :: [Char]
goingUp = ['\x030e', '\x0304', '\x0305', '\x033f', '\x0311', '\x0306',
    '\x0310', '\x0352', '\x0357', '\x0307', '\x0308', '\x030a', '\x0342', '\x0343',
    '\x0344', '\x034a', '\x034b', '\x034c', '\x0302', '\x030c', '\x0350', '\x0300',
    '\x0301', '\x030b', '\x030f', '\x0312', '\x0313', '\x033d', '\x0309', '\x0363',
    '\x0364', '\x0365', '\x0366', '\x0367', '\x0368', '\x0369', '\x036b', '\x036c',
    '\x036d', '\x036e', '\x036f', '\x033e', '\x035b', '\x0346', '\x031a']

goingDown :: [Char]
goingDown = ['\x0317', '\x0318', '\x0319', '\x031c', '\x031d', '\x031e',
    '\x031f', '\x0320', '\x0324', '\x0326', '\x0329', '\x032a', '\x032b', '\x032c',
    '\x032d', '\x032e', '\x032f', '\x0330', '\x0332', '\x0333', '\x0339', '\x033a',
    '\x033b', '\x033c', '\x0345', '\x0347', '\x0348', '\x034d', '\x034e', '\x0353',
    '\x0354', '\x0355', '\x0356', '\x0359', '\x035a', '\x0323']

stayMiddle :: [Char]
stayMiddle = ['\x031b', '\x0340', '\x0341', '\x0358', '\x0321', '\x0322',
    '\x0327', '\x0328', '\x0334', '\x0336', '\x034f', '\x035c', '\x035d', '\x035e',
    '\x035f', '\x0360', '\x0362', '\x0338', '\x0361', '\x0489']

randomStateR :: RandomGen g => (Int, Int) -> State g Int
randomStateR bounds = state $ randomR bounds

-- An utility function I am sorely missing from Control.Monad. Apply the monadic action
-- n times successively. TODO: Put this into a utility module.
chainM :: Monad m => Int -> (a -> m a) -> (a -> m a)
chainM times f = iterate (>=> f) f !! times

selectFrom :: RandomGen g => [a] -> Int -> State g [a]
selectFrom list count = chainM count f []
    where
        len = length list
        f l = let append index = l ++ [list !! index]
            in append <$> randomStateR (0, len - 1)

-- Should I use Text here instead of String? Definitely. Why don't I? Because Text does
-- not offer the required primitives, so the only solution, as far as I can see, would 
-- be to split the text up into individual characters, map those into lists, and then 
-- recombine, which utterly defeats the point (apart from being way uglier to write).
zalgify :: RandomGen g => Parameters -> String -> State g String
zalgify (Parameters l) t = foldM f [] t
    where
        combined = goingUp ++ goingDown ++ stayMiddle
        f s char = let append diacritics = s ++ (char : diacritics) 
            in append <$> (randomStateR (1, l) >>= selectFrom combined)
