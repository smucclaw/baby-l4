-- Timed Model Checker
-- converting a Timed Automaton and a CTL formula to an SMT proof problem

module TimedMC where

import L4.Syntax
import SyntaxManipulation (conjExpr, disjExpr, implExpr, abstractQ, abstractF, liftVarBy, conjsExpr, disjsExpr, applyVars, mkVarE, mkEq, mkFloatConst, mkFunTp, index, indexListFromTo, gteExpr, eqExpr, mkIntConst, liftVar, funArgsToApp, notExpr, liftType)

import PrintProg (renameAndPrintExpr, renameExpr, printExpr)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pPrint)
import Exec (reduceExpr)
import Smt (proveExpr, constrProofTarget)
import L4.KeyValueMap
    (ValueKVM(..), selectOneOfInstr, getAssocOfPathValue, hasPathValue)

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

-- name of action / delay transition relation (defined according to transition rel of TA)
actionTransitionName :: String
actionTransitionName = "ATrans"
delayTransitionName :: String
delayTransitionName = "DTrans"

-- name of action / delay transition trace relation (declared, no definition)
actionTransitionTraceName :: String
actionTransitionTraceName = "ATransTrace"
delayTransitionTraceName :: String
delayTransitionTraceName = "DTransTrace"


-- name of action / delay transition relation together with trace 
-- (defined as the conjunction of the above)
actionTransitionWithTraceName :: String
actionTransitionWithTraceName = "ATransWithTrace"
delayTransitionWithTraceName :: String
delayTransitionWithTraceName = "DTransWithTrace"


{-
actionTransTrace :: Var (Tp())
actionTransTrace  =
  let t = mkFunTp [StateT, StateT] BooleanT
  in GlobalVar (QVarName t actionTransitionName)
-}

-- the Int parameter is the number of clock variables
transitionFunTp :: Int -> Tp()
transitionFunTp n = mkFunTp ((StateT : replicate n TimeT) ++ (StateT : replicate n TimeT)) BooleanT

traceFunTp :: Int -> Tp()
traceFunTp n = mkFunTp ((IntegerT : StateT : replicate n TimeT) ++ (StateT : replicate n TimeT)) BooleanT

-- TODO: function still needed?
transTrace :: String -> Int -> Var (Tp())
transTrace transname n =
  let t = traceFunTp n
  in GlobalVar (QVarName t transname)

stepPar :: Int -> Var (Tp ())
stepPar = LocalVar (QVarName IntegerT  "step")

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

{- Currently not used
clockOfVar :: Var t -> Clock
clockOfVar v = Clock (nameOfQVarName (nameOfVar v))

toClockVarAssoc :: Var t -> (Clock, Var t)
toClockVarAssoc v = (Clock (nameOfQVarName (nameOfVar v)), v)
-}

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

{-
goalFun :: TA (Tp ()) ->  Expr (Tp ()) -> Expr (Tp ())
goalFun ta e =
  let n = length (clocksOfTA ta) + 1
      st = locPar (n - 1)
      cls = clockPars 0 (n - 2)
      sclvs  = (st : cls)
  in abstractF sclvs (goalForm ta st cls e)
-}


-- Function abstracting over (state, clocks)
initialForm :: TA (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
initialForm ta e =
  let n = length (clocksOfTA ta) + 1
      st = locPar (n - 1)
      cls = clockPars 0 (n - 2)
      sclvs  = (st : cls)
      sclvs' = indexListFromTo n (2 * n - 1) sclvs
      goal = goalForm ta st cls e
      varTp = mkFunTp (StateT : replicate (n-1) TimeT) BooleanT
      delayTransitionVar = GlobalVar (QVarName varTp delayTransitionName)
  in abstractF sclvs
      (disjExpr goal
                (abstractQ Ex sclvs (conjExpr (applyVars delayTransitionVar (sclvs' ++ sclvs)) goal)))


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
-- Function abstracting over (state, clocks), where also fnv is a fun abstracting over (state, clocks)
constrExpansionStep :: Integer -> TA (Tp()) -> Bool -> Quantif -> Quantif -> Expr (Tp()) -> Expr (Tp())
constrExpansionStep k ta genTrace pQuantif sQuantif fnv =
  let n = length (clocksOfTA ta) + 1
      st  = locPar (n - 1)
      cls = clockPars 0 (n - 2)
      sclvs = st : cls
      relativOp = case pQuantif of All -> implExpr; Ex -> conjExpr
      composOp = case sQuantif  of All -> conjExpr; Ex -> disjExpr
      trans = if genTrace
              then reduceExpr (funArgsToApp (constrTransitionsNamed ta genTrace) (ValE IntegerT (IntV k) : map mkVarE (map (liftVarBy 0 n) sclvs ++ sclvs)))
              else reduceExpr (funArgsToApp (constrTransitionsNamed ta genTrace) (map mkVarE (map (liftVarBy 0 n) sclvs ++ sclvs)))
  in abstractF sclvs
        (composOp (reduceExpr (funArgsToApp fnv (map mkVarE sclvs)))
                  (abstractQ Ex sclvs
                      (relativOp trans
                                 (reduceExpr (funArgsToApp fnv (map mkVarE sclvs))))))

-- Relation composition: for sclvs = [s, c1 .. cn], construct
-- \s \c1 .. \cn \s' \c1' .. \cn'  -> exists \s'' \c1'' .. \cn''. r1 s c1 .. cn s'' c1'' .. cn'' && r2 s'' c1'' .. cn'' s' c1' .. cn'
constrCompos :: [Var (Tp())] -> Expr (Tp()) -> Expr (Tp()) -> Expr (Tp())
constrCompos sclvs r1 r2 =
  let n = length sclvs
      scls   = indexListFromTo (2 * n) (3 * n - 1) sclvs
      scls'  = indexListFromTo n (2 * n - 1) sclvs
      scls'' = indexListFromTo 0 (n - 1) sclvs
  in abstractF (sclvs ++ sclvs)
        (abstractQ Ex sclvs
          (conjExpr (funArgsToApp r1 (map mkVarE ( scls ++ scls'' ) ))
                    (funArgsToApp r2 (map mkVarE ( scls'' ++ scls' ) ) )))


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

-- construct an action transition formula for a single transition of the TA
constrSingleActionTransition :: TA (Tp()) -> Var (Tp()) ->[Var (Tp())] -> Var (Tp()) ->[Var (Tp())] -> Transition (Tp()) -> Expr (Tp())
constrSingleActionTransition ta st cls st' cls' t =
 conjsExpr [
    mkEq st (varOfLoc (sourceOfTransition t)),
    mkEq st' (varOfLoc (targetOfTransition t)),
    guardToExpr (zip (clocksOfTA ta) cls) (guardOfTransition t),
    actionToExpr (zip (clocksOfTA ta) cls) (zip (clocksOfTA ta) cls') (clocksOfTA ta) (actionOfTransition t),
    conjsExpr (concatMap (map (clockConstrToExpr (zip (clocksOfTA ta) cls'))) [cnstrts | (l, cnstrts) <- invarsOfTA ta, l == targetOfTransition t ])
  ]

-- construct an action transition formula for all transitions of the TA
constrActionTransition :: TA (Tp ()) -> Var (Tp ()) -> [Var (Tp ())] -> Var (Tp ()) -> [Var (Tp ())] -> Expr (Tp ())
constrActionTransition ta st cls st' cls' =
  disjsExpr (map (constrSingleActionTransition ta st cls st' cls') (transitionsOfTA ta))

transitionFunParams :: TA t -> (Int, Var (Tp ()), Var (Tp ()), [Var (Tp ())], [Var (Tp ())])
transitionFunParams ta =
  let n = length (clocksOfTA ta) + 1
  in ( n
     , locPar (2 * n - 1)           -- st
     , locPar (n - 1)               -- st'
     , clockPars n (2 * n - 2)      -- cls
     , clockPars 0 (n - 2)          -- cls'
    )

defineActionTransition :: TA (Tp ()) -> VarDefn (Tp ())
defineActionTransition ta =
  let (n, st, st', cls, cls') = transitionFunParams ta
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
      actionFun = abstractF (sclvs ++ sclvs') (constrActionTransition ta st cls st' cls')
      funTp = transitionFunTp (n - 1)
  in VarDefn funTp actionTransitionName (liftType funTp) actionFun

defineDelayTransition :: TA (Tp ()) -> VarDefn (Tp ())
defineDelayTransition ta =
  let (n, st, st', cls, cls') = transitionFunParams ta
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
      delayFun = abstractF (sclvs ++ sclvs') (constrDelayTransition ta st cls st' cls')
      funTp = transitionFunTp (n - 1)
  in VarDefn funTp delayTransitionName (liftType funTp) delayFun

defineTransition :: TA (Tp ()) -> Bool -> String -> String -> String -> VarDefn (Tp ())
defineTransition ta genTrace transName transTraceName transWithTraceName =
  let (n, st, st', cls, cls') = transitionFunParams ta
      step = stepPar (2 * n)
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
      transTp = transitionFunTp (n - 1)
      traceTp = traceFunTp (n - 1)
      transFun = applyVars (GlobalVar (QVarName transTp transName))
                                 ((st:cls) ++ (st':cls'))
      traceFun = applyVars (GlobalVar (QVarName traceTp transTraceName))
                                 (step : (st:cls) ++ (st':cls'))
      defFun =
        if genTrace
        then abstractF (step: sclvs ++ sclvs') (conjExpr transFun traceFun)
        else abstractF (sclvs ++ sclvs') transFun
  in VarDefn traceTp transWithTraceName (liftType traceTp) defFun


-- transition formula with transition relations fully expanded
constrTransitionsForm :: TA (Tp()) -> Expr (Tp())
constrTransitionsForm ta =
  let (_, st, st', cls, cls') = transitionFunParams ta
      sclvs  = (st : cls)
      sclvs' = (st' : cls')
  in abstractF (sclvs ++ sclvs')
      (disjExpr (constrActionTransition ta st cls st' cls') (constrDelayTransition ta st cls st' cls'))

-- transition formula with reference to transition name using *composition* of relations
-- Function abstracting over (step, state, clocks, state', clocks')
constrTransitionsNamed :: TA (Tp()) -> Bool -> Expr (Tp())
constrTransitionsNamed ta genTrace =
  let n = length (clocksOfTA ta) + 1
      step = stepPar (3 * n)  -- the step variable is used from within the relation composition
      st = locPar (n - 1)
      cls = clockPars 0 (n - 2)
      sclvs  = (st : cls)
      varTp = traceFunTp (n - 1)
      actionTransitionWOTrace = mkVarE (GlobalVar (QVarName varTp actionTransitionName))
      delayTransitionWOTrace = mkVarE (GlobalVar (QVarName varTp delayTransitionName))
      actionTransitionWTrace = applyVars (GlobalVar (QVarName varTp actionTransitionWithTraceName)) [step]
      delayTransitionWTrace = applyVars (GlobalVar (QVarName varTp delayTransitionWithTraceName)) [step]
  in
    if genTrace
    then abstractF [step] (constrCompos sclvs delayTransitionWTrace actionTransitionWTrace)
    else constrCompos sclvs delayTransitionWOTrace actionTransitionWOTrace

{-
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
-}


-- Function abstracting over (state, clocks)
kFoldExpansion ::  Integer -> TA (Tp()) -> Bool -> Quantif -> Quantif ->  Expr (Tp()) -> Expr (Tp())
kFoldExpansion k ta genTrace pQuantif sQuantif e =
  if k == 0
  then initialForm ta e
  else constrExpansionStep k ta genTrace pQuantif sQuantif (kFoldExpansion (k-1) ta genTrace pQuantif sQuantif e)

-- create formula to model check automaton by k-fold expansion of a formula
goalSpecificForm :: Integer -> TA (Tp()) -> Bool -> Expr (Tp()) -> Expr (Tp())
goalSpecificForm k ta genTrace e =
  case e of
    UnaOpE _ (UTemporal tempOp) eBody ->
      let (pQuantif, sQuantif) = case tempOp of
            UTAF -> (All, Ex)
            UTAG -> (All, All)
            UTEF -> (Ex, Ex)
            UTEG -> (Ex, All)
          expansion = kFoldExpansion k ta genTrace pQuantif sQuantif eBody
          initialLoc = VarE StateT (varOfLoc (initialLocOfTA ta))
          initialCls = replicate (length (clocksOfTA ta)) (ValE TimeT (FloatV 0.0))
      in reduceExpr (funArgsToApp expansion (initialLoc : initialCls))
    _ -> error "in checkAutomaton: not a temporal formula"

-- Formula: (distinct loc1 .. locn)
distinctLocs :: TA (Tp()) -> Expr (Tp())
distinctLocs ta =
  let locs = locsOfTA ta
      n = length locs
      locVars = map varOfLoc locs
      varTp = mkFunTp (replicate n TimeT) BooleanT
  in applyVars (GlobalVar (QVarName varTp "distinct")) locVars

traceImplTrans :: TA (Tp()) -> String -> String -> Expr (Tp())
traceImplTrans ta transName transTraceName =
  let (n, st, st', cls, cls') = transitionFunParams ta
      step = stepPar (2 * n)
      transTp = transitionFunTp (n - 1)
      traceTp = traceFunTp (n - 1)
  in abstractQ All [step]
      (abstractQ Ex ((st : cls) ++ (st' : cls'))
        (implExpr
            (applyVars (GlobalVar (QVarName traceTp transTraceName))
                                 (step : (st:cls) ++ (st':cls')))
                      (applyVars (GlobalVar (QVarName transTp transName))
                                 ((st:cls) ++ (st':cls')))))

backgroundForms :: TA (Tp()) -> Bool -> [Expr (Tp())]
backgroundForms ta genTrace =
  let distincts = [ distinctLocs ta ]
      traceImpls =
        if genTrace
        then [ traceImplTrans ta actionTransitionName actionTransitionTraceName
             , traceImplTrans ta delayTransitionName delayTransitionTraceName]
        else []
  in distincts ++ traceImpls

--checkAutomaton :: Integer -> TA (Tp()) -> Bool -> Expr (Tp()) -> Expr (Tp())
--checkAutomaton k ta genTrace e = conjExpr (genericForms ta genTrace) (goalSpecificForm k ta genTrace e)

----------------------------------------------------------------------
-- Wiring with rest
----------------------------------------------------------------------

{-
-- Function called from the shell with:
-- stack run aut l4/aut.l4
runAut :: Program (Tp ()) -> IO ()
runAut prg =
  let ta = head (automataOfProgram prg)
      asrt = head (assertionsOfProgram prg)
      cdecls = classDeclsOfProgram prg
      globals = globalsOfProgram prg
      actTransDef = defineActionTransition ta
      delayTransDef = defineDelayTransition ta
      genTrace = True
      actTraceDef = defineTransition ta genTrace actionTransitionName actionTransitionTraceName actionTransitionWithTraceName
      delayTraceDef = defineTransition ta genTrace delayTransitionName delayTransitionTraceName delayTransitionWithTraceName
      defs = [actTransDef, delayTransDef, actTraceDef, delayTraceDef]
      instr = fromMaybe (MapVM []) (getAssocOfPathMap ["TA"] (instrOfAssertion asrt))
      config = getAssocOfPathValue ["config"] instr
      proveConsistency = True -- prove consistency
      proofTarget = checkAutomaton 1 ta genTrace (exprOfAssertion asrt)
  in proveExpr config proveConsistency cdecls globals defs proofTarget -- launching the real checker
  -- in putStrLn $ renameAndPrintExpr [] (constrTransitionsNamed ta )  -- printout of the generated formula
-}
{-
  -- TESTS:
  --in putStrLn $ renameAndPrintExpr [] proofTarget  -- printout of the generated formula
  -- in putStrLn $ renameAndPrintExpr [] (checkAutomaton 1 ta (exprOfAssertion asrt))
  -- in putStrLn $ printExpr  proofTarget
  -- in putStrLn $ renameAndPrintExpr [] (kFoldExpansion 2 ta (exprOfAssertion asrt))
  --runAut prg = putStrLn $ unlines (map constrAutTransitionTest (automataOfProgram prg))
-}

numberOfExpansions :: ValueKVM -> Integer
numberOfExpansions instr = case getAssocOfPathValue ["expansions"] instr of 
  Nothing -> 0
  Just vk -> case vk of
    IntVM n -> n
    _ -> 0

proveAssertionTA :: Program (Tp ()) -> ValueKVM -> Assertion (Tp ()) -> IO ()
proveAssertionTA prg instr asrt =
  let ta = head (automataOfProgram prg)
      cdecls = classDeclsOfProgram prg
      globals = globalsOfProgram prg
      actTransDef = defineActionTransition ta
      delayTransDef = defineDelayTransition ta
      genTrace = hasPathValue ["trace"] instr
      actTraceDef = defineTransition ta genTrace actionTransitionName actionTransitionTraceName actionTransitionWithTraceName
      delayTraceDef = defineTransition ta genTrace delayTransitionName delayTransitionTraceName delayTransitionWithTraceName
      defs = [actTransDef, delayTransDef, actTraceDef, delayTraceDef]
      config = getAssocOfPathValue ["config"] instr
      proveConsistency = selectOneOfInstr ["consistent", "valid"] instr == "consistent"
      nExpans = numberOfExpansions instr
      preconds = backgroundForms ta genTrace 
      concl = goalSpecificForm nExpans ta genTrace (exprOfAssertion asrt)
      proofTarget = constrProofTarget proveConsistency preconds concl
  in do
    print proveConsistency
    print (getAssocOfPathValue ["procs"] instr)
    proveExpr config proveConsistency cdecls globals defs proofTarget -- launching the real checker


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

constrAutTransitionTest :: TA (Tp ()) -> String
constrAutTransitionTest ta = renameAndPrintExpr [] (constrTransitionsForm ta)

constrAutGoalTest :: TA (Tp ()) -> Expr (Tp ()) -> String
constrAutGoalTest ta e = renameAndPrintExpr [] (initialForm ta e)

-- constrAutExpTest :: Integer -> TA (Tp ()) -> Expr (Tp ()) -> String
-- constrAutExpTest k ta e = renameAndPrintExpr [] (checkAutomaton k ta True e)
