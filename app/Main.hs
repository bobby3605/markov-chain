module Main where

import System.Random
import System.Environment
import Markov

usage :: IO ()
usage = do
  print "Usage: "
  print "filepath, split length, number of words, first prefix, second prefix, suffix"

main :: IO ()
main = do
  -- args!!0 = file, args!!1 = split length, args!!2 = number of words, args!!3 = first prefix, args!!4 = second prefix, args!!5 = suffix
  args <- getArgs
  if args!!0 == "-h" then usage else do
    file <- readFile $ args!!0
    g <- getStdGen
    print (chainHelper g file (read (args!!1) :: Int) (read (args!!2) :: Int) (makeChain [args!!3,args!!4] (args!!5) 1))
