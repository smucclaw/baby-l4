-- Execution / evaluation of expressions

module Exec where

import Data.List
import Syntax
import Typing
import SyntaxManipulation (mkFunTp, abstractF, abstractFvd, liftExpr, dropVar, liftType)
import PrintProg (renameAndPrintExpr, printExpr)
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pPrint)

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
  BAdiv -> div
  BAmod -> mod

bcomparFun :: BComparOp -> Val -> Val -> Bool
bcomparFun bc = case bc of
  BCeq -> (==)
  BClt -> (<)
  BClte -> (<=)
  BCgt -> (>)
  BCgte -> (>=)
  BCne -> (/=)

bboolFun :: BBoolOp -> Bool -> Bool -> Bool
bboolFun bb = case bb of
  BBimpl -> (\b1 b2 -> not b1 || b2)
  BBor -> (||)
  BBand -> (&&)

liftBArithOp :: BArithOp -> Val -> Val -> Val
liftBArithOp ba c1 c2 = case (c1, c2) of
  (IntV i1, IntV i2) -> IntV (barithFun ba i1 i2)
  _ -> ErrV

liftBComparOp :: BComparOp -> Val -> Val -> Val
liftBComparOp bc c1 c2 = BoolV (bcomparFun bc c1 c2)

liftBBoolOp :: BBoolOp -> Val -> Val -> Val
liftBBoolOp bb c1 c2 = case (c1, c2) of
  (BoolV b1, BoolV b2) -> BoolV (bboolFun bb b1 b2)
  _ -> ErrV

liftBinopExpr :: Tp() -> BinOp -> EvalResult (Tp()) -> EvalResult (Tp()) -> Expr (Tp())
liftBinopExpr t bop (ExprResult e1) (ExprResult e2) = case (bop, e1, e2) of
    (BArith ba, ValE t1 c1, ValE t2 c2) -> ValE t (liftBArithOp ba c1 c2)
    (BCompar bc, ValE t1 c1, ValE t2 c2) -> ValE t (liftBComparOp bc c1 c2)
    (BBool bb, ValE t1 c1, ValE t2 c2) -> ValE t (liftBBoolOp bb c1 c2)
    _ -> BinOpE t bop e1 e2
liftBinopExpr t bop _ _ = ValE ErrT ErrV

data EvalResult t
  = ExprResult (Expr t)
  | ClosResult [EvalResult t] (VarDecl t) (Expr t)
    deriving (Eq, Ord, Show, Read)
type ReductEnv t = [EvalResult t]

lookupEnv :: Int -> [t] -> Maybe t
lookupEnv i env = if i < length env then Just (env!!i) else Nothing

-- Evaluates an expression like a functional language. 
-- The result of an expression of functional type is a closure.
evalExpr :: ReductEnv (Tp()) -> Expr (Tp()) -> EvalResult (Tp())
evalExpr env x = case x of
  ValE t c -> ExprResult (ValE t c)
  e@(VarE _ GlobalVar {}) -> ExprResult e
  e@(VarE _t (LocalVar _vn i)) ->
    case lookupEnv i env of
      Nothing -> ExprResult e
      Just er ->  er
  UnaOpE t uop e -> ExprResult (liftUnaopExpr t uop (evalExpr env e))
  BinOpE t bop e1 e2 -> ExprResult (liftBinopExpr t bop (evalExpr env e1) (evalExpr env e2))
  IfThenElseE t ec e1 e2 -> case evalExpr env ec of
    ExprResult (ValE _ (BoolV True)) -> evalExpr env e1
    ExprResult (ValE _ (BoolV False)) -> evalExpr env e2
    _ -> ExprResult (ValE ErrT ErrV)
  AppE t f a ->
    case evalExpr env f of
      ExprResult fres -> ExprResult (AppE t fres (reduceEvalResult (evalExpr env a)))
      ClosResult clenv v fbd -> evalExpr (evalExpr env a:clenv) fbd
  FunE t v e -> ClosResult env v e
  e -> ExprResult (substClos (map reduceEvalResult env) 0 e)

reduceEvalResult :: EvalResult (Tp()) -> Expr (Tp())
reduceEvalResult (ExprResult er) = er
reduceEvalResult (ClosResult clenv v er) = substClos (map reduceEvalResult clenv) 0 (abstractFvd [v] er)

reduceExpr :: Expr (Tp()) -> Expr (Tp())
reduceExpr e = reduceEvalResult (evalExpr [] e)

substClos :: [Expr (Tp())] -> Int ->  Expr (Tp()) -> Expr (Tp())
substClos [] _ e = e
substClos env d e@ValE{} = e
substClos env d v@(VarE _ GlobalVar {}) = v
substClos env d ve@(VarE t v@(LocalVar _vn i)) =
  if i < d
  then ve
  else fromMaybe (VarE t (dropVar (length env) v)) (lookupEnv (i - d) env)
  -- Note: The env corresponds to the number of values being substituted in parallel.
  -- When substituting in a local variable of an outer scope, its index 
  -- has to be decremented by the number of intermediate vars that are eliminated.
substClos env d (UnaOpE t uop e) = UnaOpE t uop (substClos env d e)
substClos env d (BinOpE t bop e1 e2) = BinOpE t bop (substClos env d e1) (substClos env d e2)
substClos env d (IfThenElseE t ec e1 e2) = IfThenElseE t (substClos env d ec) (substClos env d e1) (substClos env d e2)
substClos env d (AppE t f a) = AppE t (substClos env d f) (substClos env d a)
substClos env d (FunE t v e) = FunE t v (substClos (map (liftExpr d) env) (d + 1) e)
substClos env d (QuantifE t q v e) = QuantifE t q v (substClos (map (liftExpr d) env) (d + 1) e)
substClos env d (FldAccE t e fn) = FldAccE t (substClos env d e) fn
substClos env d (TupleE t es) = TupleE t (map (substClos env d) es)
substClos env d (CastE t ct e) = CastE t ct (substClos env d e)
substClos env d (ListE t lop es) = ListE t lop (map (substClos env d) es)


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

{- For testing evaluation / reduction:
  for an assert (P e), reduce expression e -}
testReduction :: NewProgram (Tp ()) -> IO ()
testReduction prg =
  let a = head (assertionsOfNewProgram prg)
      e = argOfExprAppE (exprOfAssertion a)
      ev = evalExpr [] e
  in  case ev of
   ExprResult expr -> putStrLn  (printExpr expr)
   cl -> do
     pPrint cl
     putStrLn  (printExpr (reduceExpr e))

