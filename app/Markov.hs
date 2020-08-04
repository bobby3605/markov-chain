module Markov where

import Text.ParserCombinators.Parsec
import System.Random
import qualified Control.Monad.Trans.State.Lazy as S

randomChain :: Chain
randomChain = makeChain ["Random123","Random123"] "Random123" 1

type Prefix = [String] -- A prefix is one or more words
type Suffix = String  -- A suffix is only one word
type Rate = Float -- Rate at which a suffix occurs after a prefix

-- List of prefix words, their suffix, and their rate
data Chain = Chain {
  prefix :: Prefix,
  suffix :: Suffix,
  rate :: Rate
  } deriving (Eq)

instance Show Chain where
  show (Chain p s r) = "Prefix: " ++ show p ++ " Suffix: " ++ show s ++ " Rate: " ++ show r ++ "\n"

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

-- Calculates a list of the rate of each word appearing in a list of strings
wordRate :: Eq a => [a] -> [(a,Rate)]
wordRate input = zip uniques rateList -- Combines the rateList and uniques
  where uniques = uniqueWords input -- List of unique words
        numOfWords = length input -- Total number of words
        wordCounts = map (wordCounter input) uniques -- Returns a [(String,Integer)] where String is a word and Integer is the amount of times it appears
        rateList = map (\x -> x / (fromIntegral numOfWords :: Float)) (map (\b -> (fromIntegral b :: Float)) wordCounts) -- Returns a [Float] of word rates in the order they appear in uniques

-- Returns a list of unique words
uniqueWords :: Eq a => [a] -> [a]
uniqueWords input = helper input []
  where
    helper [] acc = acc -- Return the accumulator when finished
    helper (x:xs) acc = if found x acc then helper xs acc else helper xs (acc++[x]) -- If x is in the accumulator, continue, if x is not in the accumulator (haven't added it yet), add it to accumulator, then continue
    found word list = if (wordCounter list word) /= 0 then True else False -- Check if a word is in a list

-- Word count
wordCounter :: Eq a => [a] -> a -> Integer
wordCounter input word = sum $ zipWith wordCompare input (repeat word) -- Every time the word appears, a 1 is added to the list, every time it doesn't, a 0. This is summed to produce a word count

-- Bool -> Int
wordCompare :: Eq a => a -> a -> Integer
wordCompare input word = if input == word then 1 else 0

-- Generates a list of chains from an input string and number of prefixes
chainGenerator :: String -> Int -> [Chain]
chainGenerator input prefixNum = clist
  where parsed = parseByWords input -- get parsed input
        prefixes :: [Prefix]
        prefixes = splitBy parsed prefixNum -- get total list of prefixes
        suffixes :: [Suffix]
        suffixes = concatMap (\x -> getSuffixes x parsed) prefixes -- get total list of suffixes
        pslist :: [(Prefix,Suffix)]
        pslist = zip bigprefixes suffixes -- Since suffixes might be > prefixes, need to match the set of prefixes with suffixes, do this by increasing number of prefixes by amount of its suffix
          where suffixlengths :: [Int]
                suffixlengths = map (\b -> (length (getSuffixes b parsed))) prefixes -- Get list of lengths of each suffix
                bigprefixes :: [Prefix]
                bigprefixes = generator repMap prefixes []  -- Get list of prefixes with extra added
                repMap :: [(Prefix -> [Prefix])]
                repMap = map (\c -> replicate c) suffixlengths -- Create a list of functions that take a prefix and generate a list of prefixes equal to the length of their suffixes
                generator :: [(Prefix -> [Prefix])] -> [Prefix] -> [Prefix] -> [Prefix]
                generator [] _ acc = acc
                generator _ [] acc = acc
                generator (f:fs) (x:xs) acc = generator fs xs (acc++(f x)) -- recursively apply the functions to generate the list of prefixes, works similar to map
        rateList :: [((Prefix, Suffix),Rate)]
        rateList = wordRate pslist -- Generate list of prefixes, suffixes and their rates
        clist :: [Chain]
        clist = map (\x -> makeChain (getP x) (getS x) (getR x)) rateList -- Create chains from rateList
        -- Functions to extract values from rateList
        getP :: ((Prefix, Suffix), Rate) -> Prefix
        getP ((p,_),_) = p
        getS :: ((Prefix, Suffix), Rate) -> Suffix
        getS ((_,s),_) = s
        getR :: ((Prefix, Suffix), Rate) -> Rate
        getR ((_,_),r) = r

-- Creates a chain from a prefix, suffix, and rate
makeChain :: Prefix -> Suffix -> Rate -> Chain
makeChain p s r = Chain {prefix = p, suffix = s, rate = r}

-- Takes an arbitrary length sequence of strings and a larger string, gets each word after each occurance of the sequence
getSuffixes :: Prefix -> [String] -> [Suffix]
getSuffixes p input = helper p input []
  where helper :: Prefix -> [String] -> [Suffix] -> [Suffix]
        helper _ [_] acc = acc
        helper _ [] acc = acc
        --                  if the prefix == current string position, then append the next word to the suffix list,                                                       if strings don't match, continue to next string
        --                                                                                This if statement checks for out of bounds list indexing
        helper px xs acc = if px == (take (length px) xs) then helper px (tail xs) (acc++(if (drop (length px) xs) /= [] then ((head (drop (length px) xs)):[]) else [])) else helper px (tail xs) acc

-- Splits a list of strings by number
splitBy :: [a] -> Int -> [[a]]
splitBy [] _ = []   -- Splitting by 1s causes infinite recursion
splitBy input num = (take num input):(splitBy (drop (if num /= 1 then (num-1) else num) input) num)

-- Functions to extract values from a Chain
getPrefix :: Chain -> Prefix
getPrefix (Chain p _ _) = p

getSuffix :: Chain -> Suffix
getSuffix (Chain _ s _) = s

getRate :: Chain -> Rate
getRate (Chain _ _ r) = r

-- Takes a list of rates and a random number, outputs a random weighted choice
getRandom :: [Rate] -> Float -> [Bool]
getRandom rateList randomNum = listGen
  where listGenHelper :: [Rate] -> [Bool] -> Float -> [Bool]
        listGenHelper [] acc _ = acc
        listGenHelper (x:xs) acc acc2 = listGenHelper xs (acc ++ (if randomNum <= (x+acc2) && randomNum > acc2 then True:[] else False:[])) (acc2+x)
        listGen = listGenHelper rateList [] 0.00

getNextChains :: [Chain] -> Chain -> [Chain]
getNextChains list input = getSuffixList list input []
  where getSuffixList :: [Chain] -> Chain -> [Chain] -> [Chain]
        getSuffixList [] _ acc = acc
        getSuffixList (chain:chains) inputChain acc = if (getPrefix chain) == ((last (getPrefix inputChain)):[]++(getSuffix inputChain):[]) then getSuffixList chains inputChain acc++[chain] else getSuffixList chains inputChain acc

pickChain :: [Chain] -> Float -> [Chain] -> Chain
pickChain chains rand clist = helper chainBool chainsFixed
  -- Rand is [0,1). getRate (chains) may not be.
  -- Multiply the rates by a constant factor to change the range of rates to [0,1). x(sum(rates)) = 1. x = 1/(sum(rates)).
  where rangefix = 1.0/(sum (map getRate chains))
        chainsFixed = map (\a -> makeChain (getPrefix a) (getSuffix a) (rangefix*(getRate a))) chains
        chainBool = getRandom (map getRate chainsFixed) rand
        helper :: [Bool] -> [Chain] -> Chain
        -- If chain isn't found, return a random chain
        helper [] _ = pickChain clist rand clist
        helper _ [] = pickChain clist rand clist
        -- Return the correct chain from the list of Bools
        helper (headBool:tailBool) (headChain:tailChain) = if headBool then headChain else helper tailBool tailChain

-- Gets the previous chain from the state,
-- Adds 1 to the index for random numbers,
-- Puts the generated chain in the state,
-- Returns the generated chain
chainState :: [Chain] -> [Float] -> S.State (Chain,Int) Chain
chainState clist rand = do
  (prev,i) <- S.get
  let new = pickChain (getNextChains clist prev) (rand!!i) clist
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
  seed:(stateHelper (chainState clist (randoms g :: [Float])) (seed,1))

chainHelper :: StdGen -> String -> Int -> Int -> Chain -> String
chainHelper g input splitNum len seed = let clist = markovChain g input splitNum seed in
  helper++(concatMap (\a -> " "++getSuffix a) $ take len (if seed /= randomChain then clist else (drop 1 clist)))
  where helper :: String
        -- Appends prefix to output
        helper = if seed /= randomChain then (tail $ concatMap (" "++) (getPrefix seed)) else ""
