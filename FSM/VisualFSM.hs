module VisualFSM where

import FSM
import Text.Dot
import qualified Data.Map as M

fsmToDot :: FSM a b c -> Dot ()
fsmToDot a = if (verifyFSM a) then valid else error "Invalid FSM" where
    valid = netlistGraph attributesForEachNode outEdgesLeavingEachNode netList
    
    attributesForEachNode :: String -> [(String, String)]
    attributesForEachNode b = ("label", name $ (M.!) namesToStates b) : start_state
        where start_state = if (cur a == b) then [("shape", "doublecircle")] else [("shape", "circle")]
    
    outEdgesLeavingEachNode :: String -> [String]
    outEdgesLeavingEachNode b = map src all_edges
        where all_edges = filter (\val -> dst val == b) (concat $ M.elems statesToTransitions)
    
    netList :: [(String, String)]
    netList = map (\(n, _) -> (n, n)) (M.assocs namesToStates)
        
    namesToStates = mapping a
    statesToTransitions = transitions a
    

dotRep :: FSM a b c -> String
dotRep a = showDot $ fsmToDot a

createDot :: FSM a b c -> FilePath -> IO ()
createDot a b = writeFile b (dotRep a)