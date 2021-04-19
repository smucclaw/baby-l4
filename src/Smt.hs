{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Smt(proveProgram) where

import Syntax
import Data.SBV
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Maybe
import Control.Monad ((<=<))


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


type TransEnv = M.Map VarName SBool
lookupTransEnv :: TransEnv -> VarName -> SBool
lookupTransEnv env vn = Data.Maybe.fromMaybe (error $ "internal error in lookupTransEnv: Var not found: " ++ show vn)
                            (M.lookup vn env)

transBBoolOp :: BBoolOp ->  SBool -> SBool -> SBool
transBBoolOp BBimpl = (.=>)
transBBoolOp BBor = (.||)
transBBoolOp BBand = (.&&)

-- TODO: non-boolean unary and binary functions
transExpr :: TransEnv -> Expr t -> Predicate
transExpr env (ValE _ (BoolV b)) = if b then return sTrue else return sFalse   -- TODO: non-boolean values
transExpr env (VarE _ (GlobalVar vn)) = return (lookupTransEnv env vn)         -- TODO: local variables
transExpr env (UnaOpE _ (UBool UBneg) e) =
    do re <- transExpr env e
       return (sNot re)
transExpr env (BinOpE _ (BBool bbop) e1 e2) =
    do re1 <- transExpr env e1
       re2 <- transExpr env e2
       return (transBBoolOp bbop re1 re2)
transExpr env (IfThenElseE _ c e1 e2) =
    do rc <- transExpr env c
       re1 <- transExpr env e1
       re2 <- transExpr env e2
       return ((rc .=> re1) .&& (sNot rc .=> re2))
-- TODO  -- catchall
transExpr env e = return sFalse

transToPredicate:: Expr t -> Predicate
transToPredicate e = do
    let varns = map nameOfVar (Set.toList (fv e))
    syms <- mapM exists varns
    let env = M.fromList (zip varns syms)
    transExpr env e

proveExpr :: Expr t ->IO ()
proveExpr = (print <=< (allSat . transToPredicate))

proveProgram :: Show t => Program t -> IO ()
proveProgram p = 
    case assertionsOfProgram p of
        [] -> error "in proveProgram: at least one assertion required"
        a:_ -> do
            putStrLn "Launching SMT solver"
            proveExpr (exprOfAssertion a)




