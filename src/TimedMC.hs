-- Timed Model Checker
-- converting a Timed Automaton and a CTL formula to an SMT proof problem

module TimedMC where

import Syntax
import Data.List
import System.Console.Haskeline (putHistory)
import Typing (funArgsToApp)
import RuleTransfo (liftVar, conjExpr, disjExpr, abstractQ, abstractF, liftVarBy, liftType, conjsExpr)
import PrintProg (renameAndPrintExpr)

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


expandEFInitial :: TA t -> Expr t -> SMTFunDef t
expandEFInitial = undefined

mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}

applyVars :: Var (Tp()) -> [Var (Tp())] -> Expr (Tp())
applyVars f args = funArgsToApp (mkVarE f) (map mkVarE args)

index :: Int -> Var t -> Var t
index newi (LocalVar n _) = LocalVar n newi
index _ v@(GlobalVar _) = v

indexList :: [Int] -> [Var t] -> [Var t]
indexList = zipWith index

indexListFromTo :: Int -> Int -> [Var t] -> [Var t]
indexListFromTo l u = indexList (reverse [l .. u])

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
  in abstractF (sclvs ++ sclvs)
        (abstractQ Ex sclvs
          (conjExpr (applyVars r1 ( indexListFromTo (2 * n) (3 * n - 1) sclvs ++ indexListFromTo 0 (n - 1) sclvs ) )
                    (applyVars r2 ( indexListFromTo 0 (n - 1) sclvs ++ indexListFromTo n (2 * n - 1) sclvs ) ) ))


constrDelayTransition :: Var (Tp()) ->[Var (Tp())] -> Expr (Tp())
constrDelayTransition stv clvs =
  let sclvs = (stv : clvs)
      n = length sclvs
      d = LocalVar (QVarName floatT  "d") 0
      dgte0 = BinOpE booleanT (BCompar BCgte) (mkVarE d) (mkFloatConst 0.0)
      clockshift = conjsExpr (zipWith (\c' c -> BinOpE booleanT (BCompar BCeq) (mkVarE c') (BinOpE floatT (BArith BAadd) (mkVarE c) (mkVarE d))) (indexListFromTo 1 (n - 1) clvs) (indexListFromTo (n + 1) (2 * n - 1) clvs))
  in abstractF (sclvs ++ sclvs)
               (conjExpr (mkCompar BCeq (index (2 * n - 1) stv) (index (n - 1) stv))
                         (abstractQ Ex [d] (conjExpr dgte0 clockshift)))

mkFunTp :: [Tp ()] -> Tp () -> Tp ()
mkFunTp ts t = foldr (FunT ()) t ts

mkCompar :: BComparOp -> Var (Tp()) -> Var (Tp()) -> Expr (Tp())
mkCompar cop v1 v2 = BinOpE booleanT (BCompar cop) (mkVarE v1) (mkVarE v2)

mkFloatConst :: Float -> Expr (Tp())
mkFloatConst f = ValE floatT (FloatV f)

myfnv :: String -> Var (Tp())
myfnv f =
  let i = integerT
      t = ClassT () (ClsNm "Time")
      b = booleanT
  in GlobalVar (QVarName (mkFunTp [i, t, t, i, t, t] b)  f)

mystv :: Var (Tp())
mystv = LocalVar (QVarName integerT  "st") 0

myclvs :: [Var (Tp())]
myclvs = [LocalVar (QVarName (ClassT () (ClsNm "Time"))  "c1") 0, LocalVar (QVarName (ClassT () (ClsNm "Time"))  "c2") 0]


constrEFStepAbstrTest :: String
constrEFStepAbstrTest = renameAndPrintExpr [] (constrEFStepAbstr (myfnv "fn") mystv myclvs (myfnv "trans"))

constrComposTest :: String
constrComposTest = renameAndPrintExpr [] (constrCompos (mystv : myclvs) (myfnv "r1") (myfnv "r2"))

constrDelayTransitionTest :: String 
constrDelayTransitionTest = renameAndPrintExpr [] (constrDelayTransition mystv myclvs)

-- >>> constrDelayTransitionTest
-- "( \\ st: Integer -> ( \\ c1: Time -> ( \\ c2: Time -> ( \\ st_0: Integer -> ( \\ c1_0: Time -> ( \\ c2_0: Time -> ((st@5==st_0@2)&&( exists d: Float. ((d@0>=0.0)&&((c1_0@2==(c1@5+d@0))&&(c2_0@1==(c2@4+d@0))))))))))))"


{-


-}

expandEFStep :: String -> Int -> TA t -> SMTAbstr t
expandEFStep = undefined

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
