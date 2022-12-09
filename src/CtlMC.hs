-- CTL Model Checker
-- exploring explicit-state, untimed model checking
-- (contrary to TimedMC which does symbolic, timed model checking)

module CtlMC where

import L4.Syntax
-- import L4.SyntaxManipulation (conjExpr, disjExpr, implExpr, abstractQ, abstractF, liftVarBy, conjsExpr, disjsExpr, applyVars, mkVarE, mkEq, mkFloatConst, mkFunTp, index, indexListFromTo, gteExpr, eqExpr, mkIntConst, liftVar, funArgsToApp, notExpr, liftType)

import L4.PrintProg (renameAndPrintExpr, renameExpr, printExpr, printARName )
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.Pretty.Simple (pPrint)
import Exec (reduceExpr)
import Smt (proveExpr, constrProofTarget)
import L4.KeyValueMap

-- Fixpoint of s under operator f
-- This is the least resp. greatest fixpoint depending on whether
-- f is increasing or decreasing
fp :: Eq a => (a -> a) -> a -> a 
fp f s = 
    let fs = f s in 
        if s == fs 
        then s 
        else fp f fs

postStates :: TA t -> Loc -> [Loc]
postStates aut s = [targetOfTransition tr | tr <- transitionsOfTA aut, s == sourceOfTransition tr ]

postStatesSet :: TA t -> S.Set Loc -> S.Set Loc
--postStatesSet aut ss = S.unions (S.map (S.fromList . (postStates aut)) ss)
postStatesSet aut = S.unions . (S.map (S.fromList . (postStates aut)))

fpOpUTEG_old :: TA (Tp()) -> S.Set Loc -> S.Set Loc
fpOpUTEG_old aut t = S.filter (\s -> not (S.null (S.intersection (S.fromList (postStates aut s)) t))) t

fpOpUTEG :: TA (Tp()) -> S.Set Loc -> S.Set Loc -> S.Set Loc
fpOpUTEG aut sf = (S.intersection sf) . (postStatesSet aut)


ctlCheck :: TA (Tp()) -> Expr (Tp()) -> S.Set Loc
ctlCheck  aut (ValE _ (BoolV True)) = S.fromList (locsOfTA aut)
ctlCheck _aut (ValE _ (BoolV False)) = S.empty
ctlCheck  aut ve@VarE{} = S.fromList [lc | lc <- locsOfTA aut, (lc, ve)  `elem` labellingOfTA aut]
ctlCheck aut (UnaOpE _ (UBool UBnot) e) = S.difference (S.fromList (locsOfTA aut)) (ctlCheck aut e)
ctlCheck aut (BinOpE _ (BBool BBand) e1 e2) = S.intersection (ctlCheck aut e1) (ctlCheck aut e2)
ctlCheck aut (BinOpE _ (BBool BBor) e1 e2) = S.union (ctlCheck aut e1) (ctlCheck aut e2)
--ctlCheck aut (UnaOpE _ (UTemporal UTEG) e) = fp (fpOpUTEG_old aut) (ctlCheck aut e)
ctlCheck aut (UnaOpE _ (UTemporal UTEG) e) = let sf = (ctlCheck aut e) in fp (fpOpUTEG aut sf) sf
ctlCheck _aut _ = S.empty


setOkT :: Functor f => f (Tp()) -> f (Tp())
setOkT = fmap (\_t -> OkT)

proveAssertionCTL :: Program (Tp ()) -> ValueKVM -> Assertion (Tp ()) -> IO ()
proveAssertionCTL prg _instr asrt =
  let ta = head (automataOfProgram prg) in
  let expr_w_types = (exprOfAssertion asrt) in
  let expr_wo_types = setOkT expr_w_types in
    do
        putStrLn ("Launching CTL checker on " ++ printARName (nameOfAssertion asrt))
        putStrLn (show expr_w_types)
        putStrLn (show expr_wo_types)
        putStrLn (show (labellingOfTA ta))
        putStrLn (show (ctlCheck ta expr_wo_types))
        -- putStrLn (show  (fpOpUTEG ta (S.fromList [Loc {nameOfLoc = "loc0"},Loc {nameOfLoc = "loc1"}])))
        if S.member (initialLocOfTA ta) (ctlCheck ta expr_wo_types)
        then putStrLn "Property satisfied"
        else putStrLn "Property not satisfied"
