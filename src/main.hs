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
    alphabet :: Set.Set Char,
    transitions :: Transitions,
    initial :: State,
    final :: States
} deriving (Eq)
instance Show FSM where
    show (FSM s a t i f) =
        intercalate "," (map ((:[]) . intToDigit) (Set.toList s)) ++ "\n"
        ++ Set.toAscList a ++ "\n"
        ++ [intToDigit i] ++ "\n"
        ++ intercalate "," (map ((:[]) . intToDigit) (Set.toList f)) ++ "\n"
        ++ intercalate "\n" (transitionsStringify t)

type State = Int
type States = Set.Set State
type Transition = (State, Char, State)
type StateTransition = Map.Map Char State
type Transitions = Map.Map State StateTransition
type StateEqClass = Map.Map State States

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
                                alphabet = Set.fromList a,
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

stateTransitionTupleToTransition :: (State, StateTransition) -> [Transition]
stateTransitionTupleToTransition (from, ts) = map st (Map.assocs ts)
    where st (via, to) = (from, via, to)

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

--------------------------------------------------
-- minimize
--------------------------------------------------
minimize :: FSM -> FSM
minimize = minimize' .addSink . removeUnreachable

removeUnreachable :: FSM -> FSM
removeUnreachable fsm = fsm {
                            states = reachable,
                            transitions = filteredTransitions,
                            final = Set.intersection reachable (final fsm)
                        }
    where
        reachable = reachableStates fsm
        filteredTransitions = Map.filterWithKey transitionFilter (transitions fsm)
        transitionFilter state _ = Set.member state reachable


reachableStates :: FSM -> States
reachableStates fsm = until isFixedPoint expand reachable
    where
        reachable = Set.fromList [initial fsm]
        isFixedPoint set = expand set == set
        expand reachable = Set.foldl addReachable reachable reachable
        addReachable acc state = Set.union acc (Set.fromList $ Map.elems $ fromMaybe Map.empty $ Map.lookup state (transitions fsm))


addSink :: FSM -> FSM
addSink fsm = do
    if null missingCombinations
    then fsm
    else
        fsm {
            states = Set.insert sink (states fsm),
            transitions = Map.unionWith Map.union (transitions fsm) missingTransitions
        }
    where
        sink = Set.findMax (states fsm) + 1
        existingCombinations = Map.toList $ Map.mapWithKey (\ k st -> head $ Map.keys st) (transitions fsm)
        allCombinations = [(src, symbol) | src <- Set.toList (states fsm), symbol <- Set.toList (alphabet fsm)]
        missingCombinations = allCombinations \\ existingCombinations
        missingTransitions = Map.fromListWith Map.union $ map (\ (from, via) -> (from, Map.fromList [(via, sink)])) missingCombinations

minimize' :: FSM -> FSM
minimize' fsm = fsm {
                    states = Set.fromList $ Map.elems nameMap,
                    initial = stateToClassID $ initial fsm,
                    final = Set.map stateToClassID (final fsm),
                    transitions = Map.fromList $ map toClassTransition $ Map.assocs transitionMap
                }
    where
        transitionMap = transitions fsm
        initialEqClasses = combineSets $ Set.partition (`Set.member` final fsm) (states fsm)
        (eqClasses, eqClassMap) = partitionClasses initialEqClasses
        stateToClass state = fromMaybe Set.empty $ Map.lookup state eqClassMap
        nameMap = createNameMap [initial fsm] 0 Map.empty

        partitionClasses :: Set.Set States -> (Set.Set States, StateEqClass)
        partitionClasses sets = do
            let eqClassMap = createStateEqClass sets
            let refined = Set.foldl (partition eqClassMap) Set.empty sets
            if refined == sets
            then (sets, eqClassMap)
            else partitionClasses refined
        partition :: StateEqClass -> Set.Set States -> States -> Set.Set States
        partition eqClassMap acc set = Set.union acc $ partitionEqClass eqClassMap fsm set
        partitionEqClass :: StateEqClass -> FSM -> States -> Set.Set States
        partitionEqClass eqClassMap fa set = Set.fromList $ Set.foldl (classifyState eqClassMap fa) [] set
        classifyState :: StateEqClass -> FSM -> [States] -> State -> [States]
        classifyState eqClassMap fa classes state = findAndMap (\ set -> stateEquivalence state (head $ Set.elems set) fa eqClassMap) (Set.insert state) (Set.singleton state) classes


        createNameMap :: [State] -> State -> Map.Map States State -> Map.Map States State
        createNameMap [] _ nameMap = nameMap
        createNameMap (state:tail) maxID nameMap = do
            let stateClass = stateToClass state
            if Map.member stateClass nameMap
            then createNameMap tail maxID nameMap
            else do
                let nameMapUpdated = Map.insert stateClass maxID nameMap
                let stateTransitions = fromMaybe Map.empty $ Map.lookup state transitionMap
                let dstStates = Map.elems stateTransitions
                createNameMap (tail ++ dstStates) (maxID + 1) nameMapUpdated

        stateToClassID :: State -> State
        stateToClassID stateID = do
            let stateClass = fromMaybe Set.empty $ Map.lookup stateID eqClassMap
            fromMaybe 0 $ Map.lookup stateClass nameMap

        toClassTransition :: (State, StateTransition) -> (State, StateTransition)
        toClassTransition (srcState, stateTransitions) = do
        let classTransitions = Map.map stateToClassID stateTransitions
        (stateToClassID srcState, classTransitions)

createStateEqClass :: Set.Set States -> StateEqClass
createStateEqClass = Set.foldl lambda Map.empty
    where
        lambda :: StateEqClass -> States -> StateEqClass
        lambda map stateSet = Map.union map $ Map.fromList $ zip (Set.toList stateSet) $ repeat stateSet

stateEquivalence :: State -> State -> FSM -> StateEqClass -> Bool
stateEquivalence s1 s2 fsm eqClassMap = all equalDstClass (Set.elems $ alphabet fsm)
    where
        s1Map = Map.findWithDefault Map.empty s1 (transitions fsm)
        s2Map = Map.findWithDefault Map.empty s2 (transitions fsm)

        equalDstClass :: Char -> Bool
        equalDstClass symbol =
            case (Map.lookup symbol s1Map, Map.lookup symbol s2Map) of
                (Just dstS1, Just dstS2) -> Map.lookup dstS1 eqClassMap == Map.lookup dstS2 eqClassMap
                (Nothing, Nothing) -> True
                (_, _) -> False

findAndMap :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
findAndMap _ _ defVal [] = [defVal]
findAndMap p f defVal (x : xs) =
    if p x
    then f x : xs
    else x : findAndMap p f defVal xs

combineSets (x,y) = Set.fromList [x, y]

