-- Execution / evaluation of expressions

module Exec where

import Data.List
import Syntax
import Typing
import SyntaxManipulation (mkFunTp, abstractF)

liftUarithOp :: UArithOp -> Val -> Val
liftUarithOp u c = case (u, c) of
  (UAminus, IntV i) -> IntV (- i)
  _ -> ErrV

liftUboolOp :: UBoolOp -> Val -> Val
liftUboolOp u c = case (u, c) of
  (UBnot, BoolV b) -> BoolV (not b)
  _ -> ErrV

liftUnaopExpr :: Tp () -> UnaOp -> EvalResult (Tp()) -> Expr (Tp())
liftUnaopExpr t uop (ExprResult e) = case (uop, e) of
  (UArith u, ValE _ c) -> ValE t (liftUarithOp u c)
  (UBool u, ValE _ c) ->  ValE t (liftUboolOp u c)
  _ -> UnaOpE t uop e
liftUnaopExpr t uop clos = ValE ErrT ErrV

barithFun :: BArithOp -> Integer -> Integer -> Integer
barithFun ba = case ba of
  BAadd -> (+)
  BAsub -> (-)
  BAmul -> (*)
  BAdiv -> (div)
  BAmod -> (mod)

liftBarithOp :: BArithOp -> Val -> Val -> Val
liftBarithOp ba c1 c2 = case (c1, c2) of
  (IntV i1, IntV i2) -> IntV (barithFun ba i1 i2)
  _ -> ErrV


liftBinopExpr :: Tp() -> BinOp -> EvalResult (Tp()) -> EvalResult (Tp()) -> Expr (Tp())
liftBinopExpr t bop (ExprResult e1) (ExprResult e2) = case (bop, e1, e2) of
    (BArith ba, ValE t1 c1, ValE t2 c2) -> ValE t (liftBarithOp ba c1 c2)
liftBinopExpr t bop _ _ = ValE ErrT ErrV


data EvalResult t
  = ExprResult (Expr t)
  | ClosResult [EvalResult t] (VarDecl t) (Expr t)
    deriving (Eq, Ord, Show, Read)
type ReductEnv t = [EvalResult t]

lookupEnv :: Int -> ReductEnv t -> Maybe (EvalResult t)
lookupEnv i env = if i < length env then Just (env!!i) else Nothing


evalExpr :: ReductEnv (Tp()) -> Expr (Tp()) -> EvalResult (Tp())
evalExpr env x = case x of
  ValE t c -> ExprResult (ValE t c)
  VarE t v@GlobalVar {} -> ExprResult (VarE t v)
  VarE _t (LocalVar _vn i) ->
    case lookupEnv i env of
      Nothing -> ExprResult (ValE ErrT ErrV)
      Just e ->  e
  UnaOpE t uop e -> ExprResult (liftUnaopExpr t uop (evalExpr env e))
  BinOpE t bop e1 e2 -> ExprResult (liftBinopExpr t bop (evalExpr env e1) (evalExpr env e2))
  IfThenElseE t ec e1 e2 -> case evalExpr env ec of 
    ExprResult (ValE _ (BoolV True)) -> evalExpr env e1
    ExprResult (ValE _ (BoolV False)) -> evalExpr env e2
    _ -> ExprResult (ValE ErrT ErrV)
  AppE t f a ->
    case evalExpr env f of
      ExprResult fres -> evalExpr [evalExpr env a] fres
      ClosResult clenv v fbd -> evalExpr (evalExpr env a:clenv) fbd
  FunE t v e -> ClosResult env v e
  e@QuantifE {} -> ExprResult e
  _ -> undefined

{-
reduceExpr :: Expr (Tp()) -> Expr (Tp())
reduceExpr e = case evalExpr [] e of
  ExprResult er -> er
  ClosResult clenv v er -> substClos clenv (abstractF [ v] er)
-}
