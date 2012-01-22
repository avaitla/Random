module FSM where

import Data.List (foldl')
import Control.Monad (msum)
import Control.Arrow ((&&&))

import qualified Data.Map as M


type StateName = String

data State a = State {
	name :: StateName,
	internalCtx :: a
} deriving (Show)

data Transition a b c = Transition {
	src :: StateName,
	dst :: StateName,
	eval :: a -> b -> Maybe c
}

instance Show (Transition a b c) where
    show trns = "(Start, End) = (" ++ show (src trns) ++ ", " ++ show (dst trns) ++ ")\n"

data FSM a b c = FSM {
    cur :: StateName,
    mapping :: M.Map StateName (State a),
    transitions :: M.Map StateName [Transition a b c]
}

instance Show (FSM a b c) where
    show (FSM a b c) = border ++ start ++ border where
        border = "\n-------------------------------------------------\n"
        start  = "  Start: " ++ show a ++ "\n  States:" ++ states ++ "\n  Transitions:" ++ trans
        states = foldl' (\accum new -> accum ++ ("\n    " ++ show new)) "" (M.keys b)
        trans = foldl' (\accum new -> accum ++ (res new)) "" (M.elems c)
        res = foldl' (\accum (Transition a b _) -> accum ++ ("\n    " ++ show a ++ " -> " ++ show b)) ""

emptyFSM :: FSM a b c
emptyFSM = FSM "" M.empty M.empty

setStartState :: StateName -> FSM a b c -> FSM a b c
setStartState a b = if (M.member a (mapping b)) then b { cur = a } else error "State Does Not Exist"

verifyFSM :: FSM a b c -> Bool
verifyFSM fsm = all id $ map (`verifyTransition` fsm) (concat $ M.elems $ transitions fsm) where

verifyTransition :: Transition a b c -> FSM a b c -> Bool
verifyTransition (Transition a b _) st = M.member a state_set && M.member b state_set where
    state_set = mapping st

addStateToFSM :: FSM a b c -> State a -> FSM a b c
addStateToFSM fsm st@(State nme _) = fsm { mapping = M.insert nme st (mapping fsm) }


addTransitionToFSM :: FSM a b c -> Transition a b c -> FSM a b c
addTransitionToFSM fsm t@(Transition src' dest' _) = if (verifyTransition t fsm) then fsm { transitions = run_insert }
    else error "States in Transition are Not Present" where
    trans = transitions fsm
    run_insert = M.insertWith (flip (++)) src' [t] trans

runFSM :: FSM a b c -> b -> (FSM a b c -> c -> FSM a b c) -> (Maybe c, FSM a b c)
runFSM fsm world_ctx func = maybe (Nothing, fsm) (Just &&& func fsm) (_runFSM fsm world_ctx) where
    _runFSM :: FSM a b c -> b -> Maybe c
    _runFSM (FSM c m t) ctx = do
        state_ctx <- M.lookup c m
        let ictx = internalCtx state_ctx
        trans <- M.lookup c t
        msum $ map (\x -> eval x ictx ctx) trans