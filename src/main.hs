#!/usr/bin/env ghc

module Main (main) where

import System.IO
import System.Environment
import Data.Char
import Data.List
import qualified Data.Set as Set


data FSM = FSM {
    states :: States,
    alphabet :: Set.Set InChar,
    transitions :: [Transition],
    initial :: State,
    final :: States
} deriving (Eq)
instance Show FSM where
    show (FSM s a t i f) =
        intercalate "," (map ((:[]) . intToDigit) (Set.toList s)) ++ "\n" ++
        concat a ++ "\n" ++
        [intToDigit i] ++ "\n" ++
        intercalate "," (map ((:[]) . intToDigit) (Set.toList f)) ++ "\n" ++
        intercalate "" (map show t)

type State = Int
type States = Set.Set State
type InChar = String
data Transition = Transition {
    from :: State,
    to :: State,
    via :: Char
} deriving (Eq)
instance Show Transition where
    show (Transition f t v) = [intToDigit f] ++ "," ++ [v] ++ "," ++ [intToDigit t] ++ "\n"


main :: IO ()
main = do
    args <- getArgs
    let (minimalize, inFile) = handleArgs args
    file <- getFile inFile
    fileContent <- hGetContents file
    let fsm = parseFSM fileContent
    if not minimalize then
        print fsm
    else do
        -- let cleanFsm = eliminateUnreachableStates fsm
        print "hello"

handleArgs :: [String] -> (Bool, String)
handleArgs [x]
    | x=="-i" = (False, "stdin")
    | x=="-t" = (True, "stdin")
    | otherwise = error "Usage: ./dka-2-mka <-i|-t> [file]"
handleArgs [x,y]
    | x=="-i" && y/="-t" = (False, y)
    | x=="-t" && y/="-i" = (True, y)
    | otherwise = error "Usage: ./dka-2-mka <-i|-t> [file]"
handleArgs _ = error "Usage: ./dka-2-mka <-i|-t> [file]"


--------------------------------------------------
-- parse input and construct FSM
--------------------------------------------------
getFile :: String -> IO Handle
getFile filename
    | filename =="stdin" = return stdin
    | otherwise = openFile filename ReadMode

parseFSM :: String -> FSM
parseFSM input = do
    let lns = lines input
    parseFSMLines lns

parseFSMLines :: [String] -> FSM
parseFSMLines (s:a:i:f:ts) = FSM {
                                states = Set.fromList $ map (digitToInt . stringToChar) (commaSplit s),
                                alphabet = Set.fromList $ map (:[]) a,
                                initial = digitToInt $ stringToChar i,
                                final = Set.fromList $ map (digitToInt . stringToChar) (commaSplit f),
                                transitions = map parseTransition (filter (not . null) ts)
                            }

commaSplit :: String -> [String]
commaSplit x
    | x == "" = []
    | otherwise = splitStr' x []
        where
            splitStr' [] r = [r]
            splitStr' (x:xs) t
                | x==','    = t:splitStr' xs []
                | otherwise = splitStr' xs (t++[x])

stringToChar :: [Char] -> Char
stringToChar cs = if not (null (tail cs)) then
                    error "invalid string"
                  else
                    head cs

parseTransition :: String -> Transition
parseTransition x = Transition
                            {
                                from = digitToInt $ stringToChar $ head $ commaSplit x,
                                to = digitToInt $ stringToChar $ head $ tail $ tail $ commaSplit x,
                                via = head $ head $ tail $ commaSplit x
                            }


--------------------------------------------------
-- parse input and construct FSM
--------------------------------------------------
