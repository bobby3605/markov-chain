module Main where

import System.Random
import System.Environment
import Markov

usage :: IO ()
usage = do
  print "Usage: "
  print "filepath, number of words to output, optional(prefixes, suffix)"

main :: IO ()
main = do
  -- args!!0 = file, args!!1 = output length, args!!(2 to n-1) = prefixes args!!n = suffixes where n = length args
  args <- getArgs
  if head args == "-h" || length args < 2 then usage else do
    file <- readFile $ head args
    g <- getStdGen
    -- Number of prefixes is always == splitNum
    let splitNum = if length args >= 4 then length (drop 2 args)-1 else 2
    if length args >= 4 then print (chainHelper g file splitNum (read (args!!1) :: Int) (makeChain (init (drop 2 args)) (last args))) else
      print $ drop 1 $ chainHelper g file splitNum (read (args!!1) :: Int) randomChain
