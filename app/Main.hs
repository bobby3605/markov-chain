module Main where

import System.Random
import System.Environment
import Markov

usage :: IO ()
usage = do
  print "Usage: "
  print "filepath, split length, number of words, first prefix, second prefix, suffix"

-- TODO Run different parts and time them to see what takes so long

main :: IO ()
main = do
  -- args!!0 = file, args!!1 = output length args!!2 = first prefix, args!!3 = second prefix, args!!4 = suffix
  args <- getArgs
  if args!!0 == "-h" then usage else do
    file <- readFile $ args!!0
    g <- getStdGen
    -- Number of prefixes is always == splitNum
    let splitNum = (length (drop 2 args)-1)
    print (chainHelper g file splitNum (read (args!!1) :: Int) (makeChain (init (drop 2 args)) (last args) 1))
