-- Timed Model Checker
-- converting a Timed Automaton and a CTL formula to an SMT proof problem

module TimedMC where

import Syntax
import SyntaxManipulation (conjExpr, disjExpr, implExpr, abstractQ, abstractF, liftVarBy, conjsExpr, disjsExpr, applyVars, mkVarE, mkEq, mkFloatConst, mkFunTp, index, indexListFromTo, gteExpr, eqExpr, mkIntConst, liftVar)

import PrintProg (renameAndPrintExpr, printExpr)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pPrint)
import Exec (evalExpr, reduceExpr, EvalResult (ExprResult))
import Annotation (typeAnnot)

data SMTFunDef t = SMTFunDef
    { nameOfSMTFun :: Var t
    , argsOfSMTFun :: [Var t]
    , bodyOfSMTFun :: Expr t
    }
  deriving (Eq, Ord, Show, Read)

data SMTAbstr t = SMTAbstr
    { nameOfSMTAbstr :: Var t
    , bodyOfSMTAbstr :: Expr t
    }
  deriving (Eq, Ord, Show, Read)

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

-- preliminary type of states. Possibly to be transformed to Int in SMT
stateT :: Tp ()
stateT = ClassT () (ClsNm "State")

-- already contained in Prelude as subtype of Float
timeT :: Tp ()
timeT  = ClassT () (ClsNm "Time")

locPar :: Var (Tp ())
locPar = LocalVar (QVarName stateT  "st") 0

clockPars :: Int -> [Var (Tp ())]
clockPars n = map (\i -> LocalVar (QVarName timeT  ("cl"++show i)) 0) [0 .. (n - 1)]

-- variable of type "state" corresponding to a location
varOfLoc :: Loc -> Var (Tp())
varOfLoc (Loc lname) = GlobalVar (QVarName stateT lname)

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

expandEFInitial :: TA t -> Expr t -> SMTFunDef t
expandEFInitial = undefined

-- construct one expansion step of fixpoint calculation.
-- example:
-- ( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> 
-- ((((fn st@2) c1@1) c2@0)||( exists st_0: Integer. ( exists c1_0: Time. ( exists c2_0: Time. (
-- ((((((trans st@5) c1@4) c2@3) st_0@2) c1_0@1) c2_0@0)&&(((fn st_0@2) c1_0@1) c2_0@0)))))))))
-- fnv: function variable (global)
-- stv: state variable (local)
-- clvs: list of clock variables (local)
-- transv: transition variable (global)
constrEFStepAbstr :: Var (Tp()) -> Var (Tp()) -> [Var (Tp())] -> Var (Tp()) -> Expr (Tp())
constrEFStepAbstr fnv stv clvs transv =
    let sclvs = stv : clvs
        n = length sclvs
        stvi = index (n - 1) stv
        clvsi = indexListFromTo 0 (n - 2) clvs
        sclvsi = stvi : clvsi
    in abstractF sclvsi
                 (disjExpr (applyVars fnv sclvsi)
                           (abstractQ Ex sclvsi
                                     (conjExpr (applyVars transv (map (liftVarBy 0 n) sclvsi ++ sclvsi))
                                               (applyVars fnv sclvsi))))

-- Relation composition: for sclvs = [s, c1 .. cn], construct
-- \s \c1 .. \cn \s' \c1' .. \cn'  -> exists \s'' \c1'' .. \cn''. r1 s c1 .. cn s'' c1'' .. cn'' && r2 s'' c1'' .. cn'' s' c1' .. cn'
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

delayTransInv :: Var (Tp ()) -> [(Clock, Var (Tp ()))] -> (Loc, [ClConstr]) -> Expr (Tp ())
delayTransInv st cls' (l, cnstrs) =
  implExpr (mkEq st (varOfLoc l))
           (conjsExpr (map (clockConstrToExpr cls') cnstrs))

-- construct delay transition, example:
-- ( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> ( \\ st_0: Integer -> 
-- ( \\ c1_0: Time -> ( \\ c2_0: Time -> ((st@5==st_0@2)&&( exists d: Float. ((d@0>=0.0)&&((c1_0@2==(c1@5+d@0))&&(c2_0@1==(c2@4+d@0))))))))))))
-- TODO: still missing: invariants of locations
constrDelayTransition :: TA (Tp ()) -> Var (Tp ()) -> [Var (Tp ())] -> Var (Tp ()) -> [Var (Tp ())] -> Expr (Tp ())
-- TA (Tp()) -> Var (Tp()) ->[Var (Tp())] -> Expr (Tp())
constrDelayTransition ta st cls st' cls' = 
--ta stv clvs =
  {-
  let sclvs = (stv : clvs)
      n = length sclvs
      d = LocalVar (QVarName floatT  "d") 0
      dgte0 = gteExpr (mkVarE d) (mkFloatConst 0.0)
      st  = index (2 * n - 1) stv
      st' = index (n - 1) stv
      cls = indexListFromTo (n + 1) (2 * n - 1) clvs
      cls' = indexListFromTo 1 (n - 1) clvs
      clockshift = conjsExpr (zipWith (\c' c -> eqExpr (mkVarE c') (BinOpE floatT (BArith BAadd) (mkVarE c) (mkVarE d))) cls' cls)
  in -- abstractF (sclvs ++ sclvs)
  -}
  let 
      d = LocalVar (QVarName floatT  "d") 0
      dgte0 = gteExpr (mkVarE d) (mkFloatConst 0.0)
      clockshift = conjsExpr (zipWith (\c' c -> eqExpr (mkVarE (liftVar 0 c')) (BinOpE floatT (BArith BAadd) (mkVarE  (liftVar 0 c)) (mkVarE d))) cls' cls)
  in              (conjsExpr [mkEq st st',
                           conjsExpr (map (delayTransInv st (zip (clocksOfTA ta) cls')) (invarsOfTA ta)),
                           abstractQ Ex [d] (conjExpr dgte0 clockshift)
                          ])


clockConstrToExpr :: [(Clock, Var (Tp()))] -> ClConstr -> Expr (Tp())
clockConstrToExpr clParMap (ClConstr cl compop i) = BinOpE booleanT (BCompar compop) (mkVarE (varOfClockMap clParMap cl)) (mkIntConst i)

guardToExpr :: [(Clock, Var (Tp()))] -> TransitionGuard (Tp ()) -> Expr (Tp ())
guardToExpr clParMap (TransitionGuard constr expr) = conjExpr (conjsExpr (map (clockConstrToExpr clParMap) constr)) expr

resetToExpr :: [(Clock, Var (Tp ()))] -> [(Clock, Var (Tp ()))] -> [Clock] -> Clock -> Expr (Tp ())
resetToExpr clvars clvars' resetcls cl =
  if cl `elem` resetcls
  then eqExpr (mkVarE (varOfClockMap clvars' cl)) (mkFloatConst 0.0)
  else mkEq (varOfClockMap clvars' cl) (varOfClockMap clvars cl)

-- TODO: take into account the cmd 
actionToExpr :: [(Clock, Var (Tp ()))] -> [(Clock, Var (Tp ()))] -> [Clock] -> TransitionAction t -> Expr (Tp ())
actionToExpr clvars clvars' allCls (TransitionAction _act resetcls _cmd) =
  conjsExpr (map (resetToExpr clvars clvars' resetcls) allCls)

constrActionTransition :: TA (Tp()) -> Var (Tp()) ->[Var (Tp())] -> Var (Tp()) ->[Var (Tp())] -> Transition (Tp()) -> Expr (Tp())
constrActionTransition ta stpar clpars stpar' clpars' t =
 conjsExpr [
    mkEq stpar (varOfLoc (sourceOfTransition t)),
    mkEq stpar' (varOfLoc (targetOfTransition t)),
    guardToExpr (zip (clocksOfTA ta) clpars) (guardOfTransition t),
    actionToExpr (zip (clocksOfTA ta) clpars) (zip (clocksOfTA ta) clpars') (clocksOfTA ta) (actionOfTransition t),
    conjsExpr (concatMap (map (clockConstrToExpr (zip (clocksOfTA ta) clpars'))) [cnstrts | (l, cnstrts) <- invarsOfTA ta, l == targetOfTransition t ])
  ]

--constrActionTransitions :: TA (Tp()) -> Var (Tp()) ->[Var (Tp())] -> Expr (Tp())
constrActionTransitions :: TA (Tp ()) -> Var (Tp ()) -> [Var (Tp ())] -> Var (Tp ()) -> [Var (Tp ())] -> Expr (Tp ())
constrActionTransitions ta st cls st' cls' = 
  disjsExpr (map (constrActionTransition ta st cls st' cls') (transitionsOfTA ta))

constrTransitions :: TA (Tp()) -> Var (Tp()) ->[Var (Tp())] -> Expr (Tp())
constrTransitions ta stv clvs =
  let sclvs = (stv : clvs)
      n = length sclvs
      st  = index (2 * n - 1) stv
      st' = index (n - 1) stv
      cls = indexListFromTo n (2 * n - 2) clvs
      cls' = indexListFromTo 0 (n - 2) clvs
  in abstractF (sclvs ++ sclvs)
      (disjExpr (constrActionTransitions ta st cls st' cls') (constrDelayTransition ta st cls st' cls'))

expandEFStep :: String -> Int -> TA t -> SMTAbstr t
expandEFStep = undefined


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

{-
expandEF :: Int -> Int -> TA t -> String -> [SMTFunDef t]-> [SMTFunDef t]
expandEF k count ta funname defs =
    if k == count
    then defs
    else expandEF k (count + 1) ta funname (expandEFStep funname count ta : defs)

checkExpansion :: [SMTFunDef t]-> IO ()
checkExpansion = undefined

-- Model check a formula (E<> phi). 
-- The formula phi is assumed to be the argument, not the modal formula
mcEFbounded :: Int -> TA t -> Expr t -> IO ()
mcEFbounded k ta phi = do
    let expansion = expandEF k 0 ta "f" [expandEFInitial ta phi]
    checkExpansion expansion
-}

-- BEGIN for testing
mystv :: Var (Tp())
mystv = LocalVar (QVarName integerT  "st") 0

mystv' :: Var (Tp())
mystv' = LocalVar (QVarName integerT  "st'") 0

myclvs :: [Var (Tp())]
myclvs = [LocalVar (QVarName (ClassT () (ClsNm "Time"))  "c1") 0, LocalVar (QVarName (ClassT () (ClsNm "Time"))  "c2") 0]

myfnv :: String -> Var (Tp())
myfnv f =
  let i = integerT
      t = ClassT () (ClsNm "Time")
      b = booleanT
  in GlobalVar (QVarName (mkFunTp [i, t, t, i, t, t] b)  f)

constrEFStepAbstrTest :: String
constrEFStepAbstrTest = renameAndPrintExpr [] (constrEFStepAbstr (myfnv "fn") mystv myclvs (myfnv "trans"))

constrComposTest :: String
constrComposTest = renameAndPrintExpr [] (constrCompos (mystv : myclvs) (myfnv "r1") (myfnv "r2"))

-- constrDelayTransitionTest :: String
-- constrDelayTransitionTest = renameAndPrintExpr [] (constrDelayTransition myTA mystv myclvs)

myTrans :: Transition (Tp ())
myTrans = Transition (Loc "loc0")
                (TransitionGuard [ClConstr (Clock "c1") BCgte 3] trueV)
                (TransitionAction Internal [Clock "c2"] (Skip OkT))
                (Loc "loc1")

myTA :: TA (Tp ())
myTA = TA OkT "myTA" [Loc "loc0", Loc "loc1"] [] [Clock "c1", Clock "c2"] [myTrans] (Loc "loc0") [(Loc "loc0", [ClConstr (Clock "c1") BClt 3]), (Loc "loc1", [ClConstr (Clock "c2") BClt 2])] []

-- constrActionTransitionTest :: String
-- constrActionTransitionTest = renameAndPrintExpr [] (constrActionTransitions myTA mystv myclvs)

----------------------------------------------------------------------
-- Wiring with rest
----------------------------------------------------------------------

constrAutTest :: TA (Tp ()) -> String
constrAutTest ta = renameAndPrintExpr []
  (constrTransitions ta locPar (clockPars (length (clocksOfTA ta))))

runAut :: NewProgram (Tp ()) -> IO ()
runAut prg = putStrLn $ unlines (map constrAutTest (automataOfNewProgram prg))

{- For testing evaluation / reduction
runAut prg =
  let a = head (assertionsOfNewProgram prg)
      e = argOfExprAppE (exprOfAssertion a)
      ev = evalExpr [] e
  in  case ev of
   ExprResult expr -> putStrLn  (printExpr expr)
   cl -> do
     pPrint cl
     putStrLn  (printExpr (reduceExpr e))
-}

-- >>> constrActionTransitionTest
-- "( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> ( \\ st_0: Integer -> ( \\ c1_0: Time -> ( \\ c2_0: Time -> ((st@5==loc0)&&((st_0@2==loc1)&&(((c1@4>=3)&&True)&&(((c1_0@1==c1@4)&&(c2_0@0==0.0))&&(c2_0@0<2)))))))))))"

{-

( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> ( \\ st_0: Integer -> ( \\ c1_0: Time -> ( \\ c2_0: Time -> 
((st@5==loc0)&&((st_0@2==loc1)&&(((c1@4>=3)&&True)&&(((c1_0@1==c1@4)&&(c2_0@0==0.0))&&(c2_0@0<2)))))))))))



-}


-- END for testing
