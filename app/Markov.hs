module Markov where

-- Current speed is about 0.0000217 seconds per word on a stock ryzen 3600

import Text.ParserCombinators.Parsec
import System.Random
import qualified Control.Monad.Trans.State.Lazy as S
import GHC.Num
import Data.List
import Control.Parallel.Strategies

randomChain :: Chain
randomChain = makeChain ["Random123","Random123"] "Random123"

type Prefix = [String] -- A prefix is one or more words
type Suffix = String  -- A suffix is only one word

-- List of prefix words, their suffix, and their rate
data Chain = Chain {
  prefix :: Prefix,
  suffix :: Suffix
  } deriving (Eq)

instance Show Chain where
  show (Chain p s) = "Prefix: " ++ show p ++ " Suffix: " ++ show s ++ "\n"

-- Splits a string into a list of strings, split at spaces
parseByWords :: String -> [String]
parseByWords input = parserHandler $ parse wordParser "" input

-- Parser for splitting strings
wordParser :: Parser [String]
wordParser = many $ do
    a <- noneOf "" -- This fixes an error about parsing empty strings
    word <- (many (letter <|> char '\'' <|> char '/'))-- a word is many letters or special characters
    _ <- many space -- after a word there is at least one space
    return $ a:word -- return the word

-- Takes the value returned from a parser out of the Either ParseError Monad
parserHandler :: Either ParseError [String] -> [String]
parserHandler parsed = helper parsed
  where helper x =
          case x of
            Left e -> ["Error: " ++ (show e :: String)]
            Right s -> s

-- Generates a list of chains from an input string and number of prefixes
chainGenerator :: String -> Int -> [Chain]
chainGenerator input prefixNum = clist
  where parsed = parseByWords input -- get parsed input
        prefixes :: [Prefix]
        prefixes = splitBy parsed prefixNum -- get total list of prefixes
        pslist :: [(Prefix,Suffix)]
        pslist = zip bigprefixes suffixes -- Since suffixes might be > prefixes, need to match the set of prefixes with suffixes, do this by increasing number of prefixes by amount of its suffix
          where suffixlengths :: [Int]
                suffixlengths = map length suffixFunc -- Get list of lengths of each suffix
                bigprefixes :: [Prefix]
                bigprefixes = reverse $ generator repMap prefixes []  -- Get list of prefixes with extra added
                repMap :: [(Prefix -> [Prefix])]
                repMap = map (\c -> replicate c) suffixlengths -- Create a list of functions that take a prefix and generate a list of prefixes equal to the length of their suffixes
                generator :: [(Prefix -> [Prefix])] -> [Prefix] -> [Prefix] -> [Prefix]
                generator [] _ acc = acc
                generator _ [] acc = acc
                generator (f:fs) (x:xs) acc = generator fs xs ((f x)++acc) -- recursively apply the functions to generate the list of prefixes, works similar to map
                suffixFunc :: [[Suffix]]
                suffixFunc = parMap rdeepseq (\x -> getSuffixes x parsed) prefixes
                suffixes :: [Suffix]
                suffixes = concat suffixFunc -- get total list of suffixes
        clist :: [Chain]
        clist = map (\x -> makeChain (getP x) (getS x))  pslist -- Create chains from rateList
        -- Functions to extract values from rateList
        getP :: (Prefix, Suffix) -> Prefix
        getP (p,_) = p
        getS :: (Prefix, Suffix) -> Suffix
        getS (_,s) = s

-- Creates a chain from a prefix, suffix, and rate
makeChain :: Prefix -> Suffix -> Chain
makeChain p s = Chain {prefix = p, suffix = s}

-- Takes a prefix and a list of words, gets the word after each occurance of the prefix
getSuffixes :: Prefix -> [String] -> [Suffix]
getSuffixes p input = reverse $ helper input []
  where prefixLength = length p
        helper :: [String] -> [Suffix] -> [Suffix]
        helper [_] acc = acc
        helper [] acc = acc
        --                  if the prefix == current prefix, then append the next prefix to the suffix list,                  if strings don't match, continue to next string
        --                                                        This if statement checks for out of bounds list indexing
        helper xs acc = if fastCompare p xs then helper tailInput (if isNull then [] else (head currentPrefix):[]++acc) else helper tailInput acc
          where tailInput = tail xs
                currentPrefix = drop prefixLength xs
                isNull = null currentPrefix

-- length xs > length ys
-- allows for fast comparison because you don't need (take (length xs) ys)
fastCompare :: [String] -> [String] -> Bool
fastCompare [] _ = True
fastCompare _ [] = False
fastCompare (x:xs) (y:ys) = if x == y then fastCompare xs ys else False

-- Splits a list of strings by number
splitBy :: [a] -> Int -> [[a]]
splitBy [] _ = []   -- Splitting by 1s causes infinite recursion
splitBy input num = (take num input):(splitBy (drop (if num /= 1 then (num-1) else num) input) num)

getNextChains :: [Chain] -> Chain -> [Chain]
getNextChains list input = getSuffixList list input []
  where getSuffixList :: [Chain] -> Chain -> [Chain] -> [Chain]
        getSuffixList [] _ acc = acc
        getSuffixList (chain:chains) inputChain acc = if (prefix chain) == ((last (prefix inputChain)):[]++(suffix inputChain):[]) then getSuffixList chains inputChain acc++[chain] else getSuffixList chains inputChain acc

pickChain :: [Chain] -> [Chain] -> Integer -> Chain
pickChain clist chains rand = if (length chains) == 0 then helper clist else helper chains
  where helper :: [Chain] -> Chain
        helper xs = xs `genericIndex` (rand `modInteger` (toInteger $ length xs))
-- Gets the previous chain from the state,
-- Adds 1 to the index for random numbers,
-- Puts the generated chain in the state,
-- Returns the generated chain
chainState :: [Chain] -> [Integer] -> S.State (Chain,Int) Chain
chainState clist rand = do
  (prev,i) <- S.get
  let new = pickChain clist (getNextChains clist prev) (rand!!i)
  S.put (new,(i+1))
  return new

-- Creates a recursively defined list of the outputs of each state
stateHelper :: S.State (a,b) a -> (a,b) -> [a]
stateHelper f arg = do
  let headF = S.runState f arg
  let prev = snd headF
  let nextS = stateHelper f prev
  (fst prev):(nextS)

-- Put everything together
markovChain :: StdGen -> String -> Int -> Chain -> [Chain]
markovChain g input splitNum seed = let clist = chainGenerator input splitNum in
  seed:(stateHelper (chainState clist (randoms g :: [Integer])) (seed,1))

chainHelper :: StdGen -> String -> Int -> Int -> Chain -> String
chainHelper g input splitNum len seed = let clist = markovChain g input splitNum seed in
  helper++(concatMap (\a -> " "++suffix a) $ take len (if seed /= randomChain then clist else (drop 1 clist)))
  where helper :: String
        -- Appends prefix to output
        helper = if seed /= randomChain then (tail $ concatMap (" "++) (prefix seed)) else ""
