-- Timed Model Checker
-- converting a Timed Automaton and a CTL formula to an SMT proof problem

module TimedMC where

import Syntax
import SyntaxManipulation (conjExpr, disjExpr, implExpr, abstractQ, abstractF, liftVarBy, conjsExpr, disjsExpr, applyVars, mkVarE, mkEq, mkFloatConst, mkFunTp, index, indexListFromTo, gteExpr, eqExpr, mkIntConst, liftVar, funArgsToApp, notExpr, liftType)

import PrintProg (renameAndPrintExpr, renameExpr, printExpr)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pPrint)
import Exec (reduceExpr)
import Smt (proveExpr)
import KeyValueMap
    ( selectAssocOfMap, selectAssocOfValue, ValueKVM(MapVM) )

{- Assumptions about TCTL formulas:
TCTL formulas are composed of:
- special/reserved variable state : Integer for encoding states. 
  State expressions have the form state == s, where s in [0 .. n] is a number designating a state
- clock variables of type Time
  Clock comparisons are boolean expressions of the form e1 == e2, 
  where e1, e2 are clock expressions of type Time and are either clock variables, floats or sums/ differences of clock expressions.
-}

----------------------------------------------------------------------
-- Handling of types / variables
----------------------------------------------------------------------

actionTransitionName = "ATrans"
delayTransitionName = "DTrans"

-- the Int parameter is the number of clock variables
actionTransTrace :: Var (Tp())
actionTransTrace  =
  let t = mkFunTp [StateT, StateT] BooleanT
  in GlobalVar (QVarName t actionTransitionName)

delayTransTrace :: Int -> Var (Tp())
delayTransTrace n =
  let t = mkFunTp ((StateT : replicate n TimeT) ++ (StateT : replicate n TimeT)) BooleanT
  in GlobalVar (QVarName t delayTransitionName)

locPar :: Int -> Var (Tp ())
locPar = LocalVar (QVarName StateT  "st")

clockPars :: Int -> Int -> [Var (Tp ())]
clockPars l u = map (\i -> LocalVar (QVarName TimeT  ("cl"++show i)) i) (reverse [l .. u])

-- variable of type "state" corresponding to a location
varOfLoc :: Loc -> Var (Tp())
varOfLoc (Loc lname) = GlobalVar (QVarName StateT lname)

-- the variables are assumed to be correctly indexed 
-- with functions such as index or indexListFromTo
varOfClock :: [Var t] -> Clock -> Var t
varOfClock ivars cl =
  fromMaybe (error ("in varOfClock: clock with name " ++ show (nameOfClock cl) ++ " not in map"))
   (find (\v -> nameOfQVarName (nameOfVar v) == nameOfClock cl) ivars)

varOfClockMap :: [(Clock, Var t)] -> Clock -> Var t
varOfClockMap ivars cl =
  fromMaybe (error ("in varOfClock: clock with name " ++ show (nameOfClock cl) ++ " not in map"))
   (lookup cl ivars)

clockOfVar :: Var t -> Clock
clockOfVar v = Clock (nameOfQVarName (nameOfVar v))

toClockVarAssoc :: Var t -> (Clock, Var t)
toClockVarAssoc v = (Clock (nameOfQVarName (nameOfVar v)), v)


----------------------------------------------------------------------
-- Construction of formulas
----------------------------------------------------------------------

-- translation of the initial formula, by translating "at loc" for a state variable loc to "st == loc"
-- TODO: treat other constructors
-- TODO: Also translate clock variables, by mapping globally declared clock variables to local variables in cls
goalForm :: TA (Tp ()) -> Var (Tp ()) -> [Var (Tp ())] -> Expr (Tp ()) -> Expr (Tp ())
goalForm ta st cls e = case e of
  c@(ValE tp val) -> c
  v@(VarE tp var) -> v
  UnaOpE tp uo ex -> UnaOpE tp uo (goalForm ta st cls ex)
  BinOpE tp bo e1 e2 -> BinOpE tp bo (goalForm ta st cls e1) (goalForm ta st cls e2)
  IfThenElseE tp ec e1 e2 -> IfThenElseE tp (goalForm ta st cls ec) (goalForm ta st cls e1) (goalForm ta st cls e2)
  AppE _ (VarE _ (GlobalVar (QVarName _ "at") ) ) (VarE _ lc) -> mkEq st lc
  AppE tp f a  -> AppE tp (goalForm ta st cls f) (goalForm ta st cls a)
  e -> error ("goalForm: case " ++ show e ++ " currently not handled")

goalFun :: TA (Tp ()) ->  Expr (Tp ()) -> Expr (Tp ())
goalFun ta e =
  let n = length (clocksOfTA ta) + 1
      st = locPar (n - 1)
      cls = clockPars 0 (n - 2)
      sclvs  = (st : cls)
  in abstractF sclvs (goalForm ta st cls e)


-- construct one expansion step of fixpoint calculation.
-- example:
-- ( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> 
-- ((((fn st@2) c1@1) c2@0)||( exists st_0: Integer. ( exists c1_0: Time. ( exists c2_0: Time. (
-- ((((((trans st@5) c1@4) c2@3) st_0@2) c1_0@1) c2_0@0)&&(((fn st_0@2) c1_0@1) c2_0@0)))))))))
-- fnv: function variable (global)
-- stv: state variable (local)
-- clvs: list of clock variables (local)
-- transv: transition variable (global)

-- Path quantifiers are: E (some branch) and A (all branches)
-- State quantifiers are: <> (eventually) and [] (always)
-- Expansion steps are:
-- - for A<>: f(j+1)(x) = f(j)(x) || All x'. Trans(x, x') --> f(j)(x')
-- - for A[]: f(j+1)(x) = f(j)(x) && All x'. Trans(x, x') --> f(j)(x')
-- - for E<>: f(j+1)(x) = f(j)(x) || Ex x'. Trans(x, x') && f(j)(x')
-- - for E[]: f(j+1)(x) = f(j)(x) && Ex x'. Trans(x, x') && f(j)(x')
-- TODO: at least for E<>, the generated formula looks redundant;
-- f(j)(x) means: goal can be reached in j steps or less, 
-- and not: can be reached in exactly j steps.
constrExpansionStep :: TA (Tp()) -> Quantif -> Quantif -> Expr (Tp()) -> Expr (Tp())
constrExpansionStep ta pQuantif sQuantif fnv =
  let n = length (clocksOfTA ta) + 1
      st  = locPar (n - 1)
      cls = clockPars 0 (n - 2)
      sclvs = st : cls
      relativOp = case pQuantif of All -> implExpr; Ex -> conjExpr
      composOp = case sQuantif  of All -> conjExpr; Ex -> disjExpr
  in abstractF sclvs
        (composOp (reduceExpr (funArgsToApp fnv (map mkVarE sclvs)))
                  (abstractQ Ex sclvs
                      (relativOp (reduceExpr (funArgsToApp (constrTransitionsNamed ta) (map mkVarE (map (liftVarBy 0 n) sclvs ++ sclvs))))
                                 (reduceExpr (funArgsToApp fnv (map mkVarE sclvs))))))

-- Relation composition: for sclvs = [s, c1 .. cn], construct
-- \s \c1 .. \cn \s' \c1' .. \cn'  -> exists \s'' \c1'' .. \cn''. r1 s c1 .. cn s'' c1'' .. cn'' && r2 s'' c1'' .. cn'' s' c1' .. cn'
{-
constrCompos :: [Var (Tp())] -> Var (Tp()) -> Var (Tp()) -> Expr (Tp())
constrCompos sclvs r1 r2 =
  let n = length sclvs
      scls   = indexListFromTo (2 * n) (3 * n - 1) sclvs
      scls'  = indexListFromTo n (2 * n - 1) sclvs
      scls'' = indexListFromTo 0 (n - 1) sclvs
  in abstractF (sclvs ++ sclvs)
        (abstractQ Ex sclvs
          (conjExpr (applyVars r1 ( scls ++ scls'' ) )
                    (applyVars r2 ( scls'' ++ scls' ) ) ))
-}

delayTransInv :: Var (Tp ()) -> [(Clock, Var (Tp ()))] -> (Loc, [ClConstr]) -> Expr (Tp ())
delayTransInv st cls' (l, cnstrs) =
  implExpr (mkEq st (varOfLoc l))
           (conjsExpr (map (clockConstrToExpr cls') cnstrs))

-- construct delay transition, example:
-- ( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> ( \\ st_0: Integer -> 
-- ( \\ c1_0: Time -> ( \\ c2_0: Time -> ((st@5==st_0@2)&&( exists d: Float. ((d@0>=0.0)&&((c1_0@2==(c1@5+d@0))&&(c2_0@1==(c2@4+d@0))))))))))))
-- TODO: still missing: invariants of locations
constrDelayTransition :: TA (Tp ()) -> Var (Tp ()) -> [Var (Tp ())] -> Var (Tp ()) -> [Var (Tp ())] -> Expr (Tp ())
constrDelayTransition ta st cls st' cls' =
  let d = LocalVar (QVarName FloatT  "d") 0
      dgte0 = gteExpr (mkVarE d) (mkFloatConst 0.0)
      clockshift = conjsExpr (zipWith (\c' c -> eqExpr (mkVarE (liftVar 0 c')) (BinOpE FloatT (BArith BAadd) (mkVarE  (liftVar 0 c)) (mkVarE d))) cls' cls)
  in  conjsExpr [mkEq st st',
                 conjsExpr ([delayTransInv st (zip (clocksOfTA ta) cls') (l, cnstrts) | (l, cnstrts) <- invarsOfTA ta, not (null cnstrts)]),
                 abstractQ Ex [d] (conjExpr dgte0 clockshift)
                 -- , applyVars (delayTransTrace (length cls)) ((st:cls) ++ (st':cls'))
                ]


clockConstrToExpr :: [(Clock, Var (Tp()))] -> ClConstr -> Expr (Tp())
clockConstrToExpr clvarsMap (ClConstr cl compop i) = BinOpE BooleanT (BCompar compop) (mkVarE (varOfClockMap clvarsMap cl)) (mkIntConst i)

guardToExpr :: [(Clock, Var (Tp()))] -> TransitionGuard (Tp ()) -> Expr (Tp ())
guardToExpr clvarsMap (TransitionGuard constr expr) = conjExpr (conjsExpr (map (clockConstrToExpr clvarsMap) constr)) expr

resetToExpr :: [(Clock, Var (Tp ()))] -> [(Clock, Var (Tp ()))] -> [Clock] -> Clock -> Expr (Tp ())
resetToExpr clvarsMap clvarsMap' resetcls cl =
  if cl `elem` resetcls
  then eqExpr (mkVarE (varOfClockMap clvarsMap' cl)) (mkFloatConst 0.0)
  else mkEq (varOfClockMap clvarsMap' cl) (varOfClockMap clvarsMap cl)

-- TODO: take into account the cmd 
actionToExpr :: [(Clock, Var (Tp ()))] -> [(Clock, Var (Tp ()))] -> [Clock] -> TransitionAction t -> Expr (Tp ())
actionToExpr clvarsMap clvarsMap' allCls (TransitionAction _act resetcls _cmd) =
  conjsExpr (map (resetToExpr clvarsMap clvarsMap' resetcls) allCls)

constrActionTransition :: TA (Tp()) -> Var (Tp()) ->[Var (Tp())] -> Var (Tp()) ->[Var (Tp())] -> Transition (Tp()) -> Expr (Tp())
constrActionTransition ta st cls st' cls' t =
 conjsExpr [
    mkEq st (varOfLoc (sourceOfTransition t)),
    mkEq st' (varOfLoc (targetOfTransition t)),
    guardToExpr (zip (clocksOfTA ta) cls) (guardOfTransition t),
    actionToExpr (zip (clocksOfTA ta) cls) (zip (clocksOfTA ta) cls') (clocksOfTA ta) (actionOfTransition t),
    conjsExpr (concatMap (map (clockConstrToExpr (zip (clocksOfTA ta) cls'))) [cnstrts | (l, cnstrts) <- invarsOfTA ta, l == targetOfTransition t ])
    -- , applyVars actionTransTrace [st, st']
  ]

constrActionTransitions :: TA (Tp ()) -> Var (Tp ()) -> [Var (Tp ())] -> Var (Tp ()) -> [Var (Tp ())] -> Expr (Tp ())
constrActionTransitions ta st cls st' cls' =
  disjsExpr (map (constrActionTransition ta st cls st' cls') (transitionsOfTA ta))

defineActionTransitions :: TA (Tp ()) -> VarDefn (Tp ())
defineActionTransitions ta =
  let n = length (clocksOfTA ta) + 1
      st  = locPar (2 * n - 1)
      st' = locPar (n - 1)
      cls = clockPars n (2 * n - 2)
      cls' = clockPars 0 (n - 2)
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
      actionFun = abstractF (sclvs ++ sclvs') (constrActionTransitions ta st cls st' cls')
  in VarDefn BooleanT actionTransitionName (liftType BooleanT) actionFun

defineDelayTransition :: TA (Tp ()) -> VarDefn (Tp ())
defineDelayTransition ta =
  let n = length (clocksOfTA ta) + 1
      st  = locPar (2 * n - 1)
      st' = locPar (n - 1)
      cls = clockPars n (2 * n - 2)
      cls' = clockPars 0 (n - 2)
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
      delayFun = abstractF (sclvs ++ sclvs') (constrDelayTransition ta st cls st' cls')
  in VarDefn BooleanT delayTransitionName (liftType BooleanT) delayFun

-- transition formula with transition relations fully expanded
constrTransitionsForm :: TA (Tp()) -> Expr (Tp())
constrTransitionsForm ta =
  let n = length (clocksOfTA ta) + 1
      st  = locPar (2 * n - 1)
      st' = locPar (n - 1)
      cls = clockPars n (2 * n - 2)
      cls' = clockPars 0 (n - 2)
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
  in abstractF (sclvs ++ sclvs')
      (disjExpr (constrActionTransitions ta st cls st' cls') (constrDelayTransition ta st cls st' cls'))

constrTransitionsNamed :: TA (Tp()) -> Expr (Tp())
constrTransitionsNamed ta =
  let n = length (clocksOfTA ta) + 1
      st  = locPar (2 * n - 1)
      st' = locPar (n - 1)
      cls = clockPars n (2 * n - 2)
      cls' = clockPars 0 (n - 2)
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
      varTp = mkFunTp (StateT : replicate (n-1) TimeT) BooleanT 
      actionTransitionVar = GlobalVar (QVarName varTp actionTransitionName)
      delayTransitionVar = GlobalVar (QVarName varTp delayTransitionName)
  in abstractF (sclvs ++ sclvs')
      (disjExpr (applyVars actionTransitionVar (sclvs ++ sclvs')) (applyVars delayTransitionVar (sclvs ++ sclvs')))

kFoldExpansion ::  Int -> TA (Tp()) -> Quantif -> Quantif ->  Expr (Tp()) -> Expr (Tp())
kFoldExpansion k ta pQuantif sQuantif e =
  if k == 0
  then goalFun ta e
  else constrExpansionStep ta pQuantif sQuantif (kFoldExpansion (k-1) ta  pQuantif sQuantif e)

-- create formula to model check automaton by k-fold expansion of a formula
checkAutomaton :: Int -> TA (Tp()) -> Expr (Tp()) -> Expr (Tp())
checkAutomaton k ta e =
  case e of
    UnaOpE _ (UTemporal tempOp) eBody ->
      let (pQuantif, sQuantif) = case tempOp of
            UTAF -> (All, Ex)
            UTAG -> (All, All)
            UTEF -> (Ex, Ex)
            UTEG -> (Ex, All)
          expansion = kFoldExpansion k ta pQuantif sQuantif eBody
          initialLoc = VarE StateT (varOfLoc (initialLocOfTA ta))
          initialCls = replicate (length (clocksOfTA ta)) (ValE TimeT (FloatV 0.0))
      in reduceExpr (funArgsToApp expansion (initialLoc : initialCls))
    _ -> error "in checkAutomaton: not a temporal formula"


----------------------------------------------------------------------
-- Wiring with rest
----------------------------------------------------------------------

runAut :: Program (Tp ()) -> IO ()
{--}
runAut prg =
  let ta = head (automataOfProgram prg)
      asrt = head (assertionsOfProgram prg)
      cdecls = classDeclsOfProgram prg
      globals = globalsOfProgram prg
      actTransDef = defineActionTransitions ta
      delayTransDef = defineDelayTransition ta
      instr = fromMaybe (MapVM []) (selectAssocOfMap "SMT" (instrOfAssertion asrt))
      config = selectAssocOfValue "config" instr
      proveConsistency = False -- prove validity
      proofTarget = notExpr (checkAutomaton 2 ta (exprOfAssertion asrt))
  in proveExpr config proveConsistency cdecls globals [actTransDef, delayTransDef] proofTarget -- launching the real checker
  --in putStrLn $ renameAndPrintExpr [] (bodyOfVarDefn  actTransDef)  -- printout of the generated formula

{-
  -- TESTS:
  --in putStrLn $ renameAndPrintExpr [] proofTarget  -- printout of the generated formula
  -- in putStrLn $ renameAndPrintExpr [] (checkAutomaton 1 ta (exprOfAssertion asrt))
  -- in putStrLn $ printExpr  proofTarget
  -- in putStrLn $ renameAndPrintExpr [] (kFoldExpansion 2 ta (exprOfAssertion asrt))
  --runAut prg = putStrLn $ unlines (map constrAutTransitionTest (automataOfProgram prg))
-}

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

constrAutTransitionTest :: TA (Tp ()) -> String
constrAutTransitionTest ta = renameAndPrintExpr [] (constrTransitionsForm ta)

constrAutGoalTest :: TA (Tp ()) -> Expr (Tp ()) -> String
constrAutGoalTest ta e = renameAndPrintExpr [] (goalFun ta e)

constrAutExpTest :: Int -> TA (Tp ()) -> Expr (Tp ()) -> String
constrAutExpTest k ta e = renameAndPrintExpr [] (checkAutomaton k ta e)
