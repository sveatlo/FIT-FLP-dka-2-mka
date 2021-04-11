#!/usr/bin/env ghc

module Main (main) where

import System.IO
import System.Environment
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf


data FSM = FSM {
    states :: States,
    alphabet :: Set.Set InChar,
    transitions :: Transitions,
    initial :: State,
    final :: States
} deriving (Eq)
instance Show FSM where
    show (FSM s a t i f) =
        intercalate "," (map ((:[]) . intToDigit) (Set.toList s)) ++ "\n"
        ++ concat a ++ "\n"
        ++ [intToDigit i] ++ "\n"
        ++ intercalate "," (map ((:[]) . intToDigit) (Set.toList f)) ++ "\n"
        ++ intercalate "\n" (transitionsStringify t)

type State = Int
type States = Set.Set State
type InChar = String
type Transition = (State, Char, State)
type StateTransition = Map.Map Char State
type Transitions = Map.Map State StateTransition


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
        -- let cleanFSM = removeUnreachable fsm
        -- let fullFSM = addSink cleanFSM
        -- let minimalFSM = minimize fullFSM
        print $ minimize fsm

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
    if length lns < 4 then error "invalid input"
    else
        parseFSMLines lns

parseFSMLines :: [String] -> FSM
parseFSMLines (s:a:i:f:ts) = FSM {
                                states = Set.fromList $ map (digitToInt . stringToChar) (commaSplit s),
                                alphabet = Set.fromList $ map (:[]) a,
                                initial = digitToInt $ stringToChar i,
                                final = Set.fromList $ map (digitToInt . stringToChar) (commaSplit f),
                                transitions = createTransitions $ map parseTransition (filter (not . null) ts)
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
parseTransition x = (
                        digitToInt fromCh,
                        via,
                        digitToInt toCh
                    )
    where
        fromCh = stringToChar $ head $ commaSplit x
        via = head $ head $ tail $ commaSplit x
        toCh = stringToChar $ head $ tail $ tail $ commaSplit x

createTransitions :: [Transition] -> Transitions
createTransitions transitionsList = createTransitionsMap $ map transitionToStateTransitionTuple transitionsList

transitionToStateTransitionTuple :: Transition -> (State, StateTransition)
transitionToStateTransitionTuple (from, via, to) = (from, Map.fromList [(via, to)])

createTransitionsMap :: [(State, StateTransition)] -> Transitions
createTransitionsMap [t] = Map.fromList [t]
createTransitionsMap ((via,stateTransition):ts) = Map.insertWith Map.union via stateTransition (createTransitionsMap ts)

--------------------------------------------------
-- print
--------------------------------------------------
transitionsStringify :: Transitions -> [String]
transitionsStringify transitions = map transitionStringify $ concatMap stateTransitionTupleToTransition (Map.assocs transitions)

transitionStringify :: Transition -> String
transitionStringify (from, via, to) = printf "%d,%c,%d" from via to

stateTransitionTupleToTransition :: (State, StateTransition) -> [Transition]
stateTransitionTupleToTransition (from, ts) = map st (Map.assocs ts)
    where st (via, to) = (from, via, to)


--------------------------------------------------
-- minimize
--------------------------------------------------
minimize :: FSM -> FSM
minimize fsm = do
    fsm
