{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


-- The following two included following type checking hints to get rid of typing errors in transToPredicateSingle
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-deferred-type-errors #-}


module SmtSBV(proveProgram) where

import Syntax
    ( Assertion(exprOfAssertion),
      BArithOp(..),
      BBoolOp(..),
      BComparOp(..),
      BinOp(..),
      Expr(..),
      Program(assertionsOfProgram, globalsOfProgram),
      Tp,
      UArithOp(..),
      UBoolOp(..),
      UnaOp(..),
      Val(IntV, BoolV),
      Var(GlobalVar, LocalVar),
      VarDecl(VarDecl, nameOfVarDecl),
      VarName )
import Annotation (TypeAnnot)
import Typing (getTypeOfExpr, isBooleanTp, isIntegerTp)

import Data.SBV
import Data.SBV.Dynamic

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Maybe
import Control.Monad ((<=<))
import Control.Monad.Trans         (MonadIO(liftIO), MonadTrans(lift))
import Data.SBV.Internals


-- returns a list of subexpressions that are not in the fragment translateable to SMT
-- If translateableToPredicate e == [], then e can be translated to SMT
translateableToPredicate :: Expr t -> [Expr t]
translateableToPredicate (ValE _ v) = []
translateableToPredicate (VarE _ v) = []
translateableToPredicate (UnaOpE _  _ e) = translateableToPredicate e
translateableToPredicate (BinOpE _  _ e1 e2) = translateableToPredicate e1 ++ translateableToPredicate e2
translateableToPredicate (IfThenElseE _ c e1 e2) = translateableToPredicate c ++ translateableToPredicate e1 ++ translateableToPredicate e2
translateableToPredicate (AppE _ f a) = []   -- TODO
translateableToPredicate (QuantifE _ _ vn t e) = translateableToPredicate e -- TODO
translateableToPredicate e = [e]


-- Extracts the set of free variables of an expression, i.e. those that are not bound by a quantifier or a lambda.
-- The names of global variables are returned; local variables are ignored.
fv :: Expr t -> Set.Set Var
fv (ValE _ v) = Set.empty
fv (VarE _ v@(GlobalVar vn)) = Set.singleton v
fv (VarE _ (LocalVar _ _)) = Set.empty
fv (UnaOpE _  _ e) = fv e
fv (BinOpE _  _ e1 e2) = Set.union (fv e1) (fv e2)
fv (IfThenElseE _ c e1 e2) = Set.unions [fv c, fv e1, fv e2]
fv (AppE _ f a) = Set.union (fv f) (fv a)
fv (FunE _ p t e) = fv e
fv (QuantifE _ _ vn t e) = fv e
fv (FldAccE _ e _) = fv e
fv (TupleE _ es) = Set.unions (map fv es)
fv (CastE _ _ e) = fv e
fv (ListE _ _ es) = Set.unions (map fv es)
fv (NotDeriv _ _ e) = fv e

data SBVType
    = TSBool SBool
    | TSInteger SInteger

-- implemented as list because new elements are added by binders
type TransEnvGen = [(VarName, SmtSBV.SBVType)]
lookupTransEnvGen :: TransEnvGen -> VarName -> SmtSBV.SBVType
lookupTransEnvGen env vn =
    Data.Maybe.fromMaybe (error $ "internal error in lookupTransEnvGen: Var not found: " ++ show vn)
    (lookup vn env)

lookupEnvSBool :: TransEnvGen -> VarName -> SBool
lookupEnvSBool env vn =
    case lookupTransEnvGen env vn of
        TSBool s -> s
        _ -> error $
                "internal error in lookupEnvSBool: "
              ++ vn
              ++ " should be boolean"

lookupEnvSInteger :: TransEnvGen -> VarName -> SInteger
lookupEnvSInteger env vn =
    case lookupTransEnvGen env vn of
        TSInteger s -> s
        _ -> error $
                "internal error in lookupEnvSInteger: "
              ++ vn
              ++ " should be integer"

type TransEnv = M.Map VarName SBool
lookupTransEnv :: TransEnv -> VarName -> SBool
lookupTransEnv env vn = Data.Maybe.fromMaybe (error $ "internal error in lookupTransEnv: Var not found: " ++ show vn)
                            (M.lookup vn env)

transBComparOp ::  (OrdSymbolic a) => BComparOp ->  a -> a -> SBool
transBComparOp  BCeq = (.==)
transBComparOp  BClt = (.<)
transBComparOp  BClte = (.<=)
transBComparOp  BCgt = (.>)
transBComparOp  BCgte = (.>=)
transBComparOp  BCne = (./=)


transBBoolOp :: BBoolOp ->  SBool -> SBool -> SBool
transBBoolOp BBimpl = (.=>)
transBBoolOp BBor = (.||)
transBBoolOp BBand = (.&&)

transBArithOp :: BArithOp ->  SInteger -> SInteger -> SInteger
transBArithOp BAadd = (+)
transBArithOp BAsub = (-)
transBArithOp BAmul = (*)
{-
transBArithOp BAdiv = div
transBArithOp BAmod= mod
  -}

transExprSInteger :: TransEnvGen -> Expr t -> SInteger
transExprSInteger env (ValE _ (IntV i)) = fromIntegral i
transExprSInteger env (ValE _ _) =
    error "internal error in transExprSInteger: non-integer value"
transExprSInteger env (VarE _ (GlobalVar vn)) =
    lookupEnvSInteger env vn
transExprSInteger env (BinOpE _ (BArith baop) e1 e2) =
    transBArithOp baop (transExprSInteger env e1) (transExprSInteger env e2)
-- TODO  -- catchall
transExprSInteger env _ = 0


-- TODO: non-boolean unary and binary functions
transExprPredicate :: TransEnvGen -> Expr t -> Predicate
transExprPredicate env (ValE _ (BoolV b)) = if b then return sTrue else return sFalse
transExprPredicate env (ValE _ _) =
    error "internal error in transExprPredicate: non-boolean value"
transExprPredicate env (VarE _ (GlobalVar vn)) =
    return (lookupEnvSBool env vn)
transExprPredicate env (UnaOpE _ (UBool UBnot) e) =
    do re <- transExprPredicate env e
       return (sNot re)
       {-
transExprPredicate env (BinOpE _ (BCompar bcop) e1 e2) =
    do re1 <- transExpr env e1
       re2 <- transExpr env e2
       return (transBComparOp bcop re1 re2)
       -}
transExprPredicate env (BinOpE _ (BCompar bcop) e1 e2) =
    let re1 = transExprSInteger env e1
        re2 = transExprSInteger env e2
    in return (transBComparOp bcop re1 re2)
transExprPredicate env (BinOpE _ (BBool bbop) e1 e2) =
    do re1 <- transExprPredicate env e1
       re2 <- transExprPredicate env e2
       return (transBBoolOp bbop re1 re2)
-- TODO: translating if-then-else as in the following only works
-- if the two branches are of type Bool.
-- To be done otherwise: some form of if-lifting
-- TODO: what about the "ite" mentioned in section "cardinality constraints"
-- of https://hackage.haskell.org/package/sbv-8.14/docs/Data-SBV.html
-- Also see Mergeable Values:
-- https://hackage.haskell.org/package/sbv-8.14/docs/Data-SBV.html#g:23
{-
transExprPredicate env (IfThenElseE _ c e1 e2) =
    do rc <- transExpr env c
       re1 <- transExpr env e1
       re2 <- transExpr env e2
       return ((rc .=> re1) .&& (sNot rc .=> re2))
-}
-- TODO  -- catchall
transExprPredicate env e = return sFalse

transExprBCompar :: TypeAnnot f => TransEnvGen -> BComparOp -> Expr (f Tp) -> Expr (f Tp) -> Predicate
transExprBCompar env bcop e1 e2 =
    let t1 = getTypeOfExpr e1
        t2 = getTypeOfExpr e2
    in
    if isBooleanTp t1 && isBooleanTp t2
    then do
        re1 <- transExprPredicate env e1
        re2 <- transExprPredicate env e2
        return (transBComparOp bcop re1 re2)
    else error "transExprBCompar"


{-
transExprPredicateOld :: TransEnv -> Expr t -> Predicate
transExprPredicateOld env (ValE _ (BoolV b)) = if b then return sTrue else return sFalse
transExprPredicateOld env _ = error "Foobar"

transToPredicateOld :: Expr t -> Predicate
transToPredicateOld e = do
    let varns = map nameOfVar (Set.toList (fv e))
    -- mapM: (a -> m b) -> t a -> m (t b)
    -- exists: SymVal c => String -> Symbolic (SBV c)
    -- varns: [VarName]
    -- mapM exists: [String] -> Symbolic (SBV c)   with: a = String, m = Symbolic, b = SBV c, t = [ .. ]
    -- not clear why c is instantiated to Bool. Because this is the first instance of SymVal?
    syms <- mapM exists varns
    let lst = zip varns syms
    let env = M.fromList lst
    transExprPredicateOld env e

proveExprOld :: Expr t ->IO ()
proveExprOld = (print <=< (allSat . transToPredicateOld))


varDeclToTransEnvGen :: VarDecl t -> SBVType
varDeclToTransEnvGen (VarDecl _ vn vt)
  | isBooleanTp vt = TSBool (exists vn)
  | isIntegerTp vt = TSInteger (exists vn)
  | otherwise = error $ "in varDeclToTransEnvGen: type " ++ show vt ++ " not supported"
-}

transExprSBool :: TransEnvGen -> Expr t -> SBool
transExprSBool env (ValE _ (BoolV b)) = if b then sTrue else sFalse
transExprSBool env (ValE _ _) =
    error "internal error in transExprPredicate: non-boolean value"
transExprSBool env (VarE _ (GlobalVar vn)) = lookupEnvSBool env vn
transExprSBool env (UnaOpE _ (UBool UBnot) e) = sNot (transExprSBool env e)
transExprSBool env _ = error "in transExprSBool"

--transToPredicateSingle:: Provable a => VarDecl t -> Expr t -> a
transToPredicateSingle:: VarDecl t -> Expr t -> IO SatResult
transToPredicateSingle (VarDecl _ vn vt) e
  | isBooleanTp vt = sat (\x -> transExprSBool [(vn, TSBool x)] e)
  | isIntegerTp vt = sat (\x -> transExprSBool [(vn, TSInteger x)] e)
  | otherwise = error "in transToPredicateSingle"

{-
transToPredicate:: [VarDecl t] -> Expr t -> Predicate
transToPredicate vds e = do
    let varns = map nameOfVarDecl vds
        syms = map exists varns
        env = M.fromList (zip varns syms)
    transExprPredicate env e
-}


type TransEnvDyn = [(VarName, SVal)]
lookupTransEnvDyn :: TransEnvDyn -> VarName -> SVal
lookupTransEnvDyn env vn =
    Data.Maybe.fromMaybe (error $ "internal error in lookupTransEnvDyn: Var not found: " ++ show vn)
    (lookup vn env)


varTypeToKind :: Tp -> Kind
varTypeToKind vt
  | isBooleanTp vt = KBool
  | isIntegerTp vt = KUnbounded
  | otherwise = error $ "in varTypeToKind: type " ++ show vt ++ " not supported"

varDeclToTransEnvDyn :: MonadSymbolic m => VarDecl t -> m SVal
varDeclToTransEnvDyn (VarDecl _ vn vt) = symbolicEnv >>= liftIO . svMkSymVar (NonQueryVar Nothing) (varTypeToKind vt) (Just vn)

proveExpr :: [VarDecl t] -> Expr t ->IO ()
-- Works for SBV.Dynamic:
-- proveExpr vds  = print <=< ((Data.SBV.Dynamic.satWith z3) . transExprDyn vds)
proveExpr vds e = do
    let tr = do
              tvds <- mapM mkTrEnvDyn vds
              transExprDyn tvds e
    benchm <- Data.SBV.Dynamic.generateSMTBenchmark True tr
    satres <- Data.SBV.Dynamic.satWith z3 tr
    putStrLn benchm
    print satres

mkTrEnvDyn :: VarDecl t -> SymbolicT IO (VarName, SVal)
mkTrEnvDyn vd = do
    x <- varDeclToTransEnvDyn vd
    pure (nameOfVarDecl vd,x)


-- proveExpr vds  = (print <=< (allSat . transToPredicate vds))
-- proveExpr vds  = (print <=< (allSat . transToPredicateSingle (head vds)))
{-
proveExpr vds e =
    do
        res <- transToPredicateSingle (head vds) e
        print res
-}

proveProgram :: Show t => Program t -> IO ()
proveProgram p =
    case assertionsOfProgram p of
        [] -> error "in proveProgram: at least one assertion required"
        a:_ -> do
            putStrLn "Launching SMT solver"
            proveExpr (globalsOfProgram p) (exprOfAssertion a)




-------------------------------------------------------------
-- Experiments with the untyped SBV.Dynamic
-------------------------------------------------------------
transUArithOpDyn :: UArithOp ->  SVal -> SVal
transUArithOpDyn UAminus = svUNeg

transUBoolOpDyn :: UBoolOp ->  SVal -> SVal
transUBoolOpDyn UBnot = svNot

transUnaOpDyn :: UnaOp -> SVal -> SVal
transUnaOpDyn (UArith ua) = transUArithOpDyn ua
transUnaOpDyn (UBool ub) = transUBoolOpDyn ub

transBArithOpDyn :: BArithOp ->  SVal  -> SVal  -> SVal
transBArithOpDyn BAadd = svPlus
transBArithOpDyn BAsub = svMinus
transBArithOpDyn BAmul = svTimes
transBArithOpDyn BAdiv = svDivide
transBArithOpDyn BAmod= svRem

transBComparOpDyn :: BComparOp ->  SVal -> SVal -> SVal
transBComparOpDyn  BCeq = svEqual
transBComparOpDyn  BClt = svLessThan
transBComparOpDyn  BClte = svLessEq
transBComparOpDyn  BCgt = svGreaterThan
transBComparOpDyn  BCgte = svGreaterEq
transBComparOpDyn  BCne = svNotEqual

transBBoolOpDyn :: BBoolOp ->  SVal -> SVal -> SVal
{- transBBoolOpDyn BBimpl = (.=>) -}
transBBoolOpDyn BBor = svOr
transBBoolOpDyn BBand = svAnd

transBinOp :: BinOp ->  SVal -> SVal -> SVal
transBinOp (BArith ba) = transBArithOpDyn ba
transBinOp (BCompar bc) = transBComparOpDyn bc
transBinOp (BBool bb) = transBBoolOpDyn bb

transExprDyn :: TransEnvDyn -> Expr t -> Symbolic SVal
transExprDyn env (ValE _ (BoolV b)) = return (svBool b)
transExprDyn env (ValE _ (IntV i)) = return (svInteger KUnbounded i)
transExprDyn env (VarE _ (GlobalVar vn)) =
    return (lookupTransEnvDyn env vn)
transExprDyn env (UnaOpE _ u e) =
    do re <- transExprDyn env e
       return (transUnaOpDyn u re)
transExprDyn env (BinOpE _ b e1 e2) =
    do re1 <- transExprDyn env e1
       re2 <- transExprDyn env e2
       return (transBinOp b re1 re2)
transExprDyn env (ValE t v) = undefined
transExprDyn env (VarE t v) = undefined
transExprDyn env (IfThenElseE t et et4 et5) = undefined
transExprDyn env (AppE t et et4) = undefined
transExprDyn env (FunE t p t4 et) = undefined
transExprDyn env (QuantifE t q varname tp et) = do
    let vd = VarDecl () varname tp
    varDeclToTransEnvDyn vd
    -- TODO: This is just nonsense
transExprDyn env (FldAccE t et f) = undefined
transExprDyn env (TupleE t l_et) = undefined
transExprDyn env (CastE t t3 et) = undefined
transExprDyn env (ListE t l l_et) = undefined
transExprDyn env (NotDeriv t b et) = undefined
       {-
transExprDyn env (BinOpE _ (BCompar bcop) e1 e2) =
    do re1 <- transExpr env e1
       re2 <- transExpr env e2
       return (transBComparOp bcop re1 re2)

transExprDyn env (BinOpE _ (BCompar bcop) e1 e2) =
    let re1 = transExprSInteger env e1
        re2 = transExprSInteger env e2
    in return (transBComparOp bcop re1 re2)
transExprDyn env (BinOpE _ (BBool bbop) e1 e2) =
    do re1 <- transExprDyn env e1
       re2 <- transExprDyn env e2
       return (transBBoolOp bbop re1 re2)
       -}
-- TODO  -- catchall
-- transExprDyn env _ = return svFalse





