module FSMExamples where
    
import FSM
import VisualFSM
import Text.Dot
import Data.List

-- This is a simplistic example, but more to come...
--   ToDo: Regex Example
--         Internal State Modification
--         

-- Our State Diagram will Look as Follows:
--  
--   -----------------------------------
--  /          /--> st3 --\             \
--  --> st1 -->            --> st4 --> st5
--             \--> st2 --/             
--
-- Here we will Let st1 be our start state
--

-- First we Generate Our State Names
[st1, st2, st3, st4, st5] = map (\val -> "State " ++ show val) [1..5]

st1_2 = Transition st1 st2 (\_ a -> if (a == "Push") then Just ToST2 else Nothing)
st1_3 = Transition st1 st3 (\_ a -> if (a == "Force") then Just ToST3 else Nothing)
st3_4 = Transition st3 st4 (\_ a -> if (a == "Cleanup") then Just ToST4 else Nothing)
st2_4 = Transition st2 st4 (\_ a -> if (a == "Cleanup") then Just ToST4 else Nothing)
st4_5 = Transition st4 st5 (\_ a -> if (a == "Steady") then Just ToST5 else Nothing)
st5_1 = Transition st5 st1 (\_ a -> if (a == "Restart") then Just ToST1 else Nothing)

data EchoMessages = ToST1 | ToST2 | ToST3 | ToST4 | ToST5

fsm :: FSM () String EchoMessages
fsm = setStartState "State 1" transitions where
    transitions = foldl' addTransitionToFSM states [st1_2, st1_3, st3_4, st2_4, st4_5, st5_1]
    states = foldl' addStateToFSM emptyFSM (map (\a -> State a ()) [st1, st2, st3, st4, st5])

example :: IO ()
example = createDot fsm "out1.gv"