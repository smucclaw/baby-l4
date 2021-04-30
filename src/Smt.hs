-- L4 to SMT interface using the SimpleSMT library

module Smt(proveProgram) where

import Syntax
import Annotation (TypeAnnot)
import Typing (getTypeOfExpr, isBooleanTp, isIntegerTp)

import SimpleSMT as SMT
import qualified Data.Maybe




-------------------------------------------------------------
-- Extensions to SExpr construction currently not in SimpleSMT
-------------------------------------------------------------

quantifToSMT :: Quantif -> String
quantifToSMT All = "forall"
quantifToSMT Ex = "exists"

quantif :: Quantif -> SExpr -> SExpr -> SExpr
quantif q vds e = fun (quantifToSMT q) [vds, e]

-- local variable reference in a quantified expression 
localVarRef :: VarName -> SExpr
localVarRef = Atom

type SMTFunEnv = [(VarName, SExpr)]


tpToSort :: Tp -> SExpr
tpToSort t
  | isBooleanTp t = tBool
  | isIntegerTp t = tInt
  | otherwise = error $ "in tpToSort: " ++ show t ++ " not supported"


varTypeToSExprTD :: VarName -> Tp -> SExpr
varTypeToSExprTD vn t = List [List [Atom vn, tpToSort t]]


varDeclToFun :: Solver -> VarDecl t -> IO (VarName, SExpr)
varDeclToFun s (VarDecl _ vn vt) = do
     se <- declare s vn (tpToSort vt)
     return (vn, se)

varDeclsToFunEnv :: Solver -> [VarDecl t] -> IO SMTFunEnv
varDeclsToFunEnv s = mapM (varDeclToFun s)


valToSExpr :: Val -> SExpr
valToSExpr (BoolV b) = bool b
valToSExpr (IntV i) = int i
valToSExpr _ = error "valToSExpr: not implemented"

-- TODO: For this to work, names (also of bound variables) have to be unique
varToSExpr :: SMTFunEnv -> Var -> SExpr
varToSExpr env (GlobalVar vn) =
    Data.Maybe.fromMaybe
        (error $ "internal error in varToSExpr: Var not found: " ++ show vn)
        (lookup vn env)
varToSExpr env (LocalVar vn i) = localVarRef vn

transUArithOp :: UArithOp ->  SExpr -> SExpr
transUArithOp UAminus = neg

transUBoolOp :: UBoolOp ->  SExpr -> SExpr
transUBoolOp UBneg = SMT.not

transUnaOp :: UnaOp -> SExpr -> SExpr
transUnaOp (UArith ua) = transUArithOp ua
transUnaOp (UBool ub) = transUBoolOp ub

transBArithOp :: BArithOp -> SExpr -> SExpr -> SExpr
transBArithOp BAadd = add
transBArithOp BAsub = sub
transBArithOp BAmul = mul
transBArithOp BAdiv = SMT.div
transBArithOp BAmod = SMT.mod

transBComparOp :: BComparOp -> SExpr -> SExpr -> SExpr
transBComparOp  BCeq = eq
transBComparOp  BClt = lt
transBComparOp  BClte = leq
transBComparOp  BCgt = gt
transBComparOp  BCgte = geq
transBComparOp  BCne = \e1 e2 -> SMT.not (eq e1 e2)

transBBoolOp :: BBoolOp -> SExpr -> SExpr -> SExpr
transBBoolOp BBimpl = implies
transBBoolOp BBor = SMT.or
transBBoolOp BBand = SMT.and

transBinOp :: BinOp -> SExpr -> SExpr -> SExpr
transBinOp (BArith ba) = transBArithOp ba
transBinOp (BCompar bc) = transBComparOp bc
transBinOp (BBool bb) = transBBoolOp bb

exprToSExpr :: SMTFunEnv -> Expr t -> SExpr
exprToSExpr env (ValE _ v) = valToSExpr v
exprToSExpr env (VarE _ v) = varToSExpr env v
exprToSExpr env (UnaOpE _ u e) = transUnaOp u (exprToSExpr env e)
exprToSExpr env (BinOpE _ b e1 e2) = transBinOp b (exprToSExpr env e1) (exprToSExpr env e2)
exprToSExpr env (IfThenElseE _ c e1 e2) = ite (exprToSExpr env c) (exprToSExpr env e1) (exprToSExpr env e2)
exprToSExpr env (QuantifE _ q vn t e) =
  quantif q (varTypeToSExprTD vn t) (exprToSExpr env e)
-- TODO: still incomplete


proveExpr :: [VarDecl t] -> Expr t ->IO ()
proveExpr vds e = do
  l <- newLogger 0
  -- s <- newSolver "cvc4" ["--lang=smt2"] (Just l)
  s <- newSolver "z3" ["-in"] (Just l)
  setLogic s "LIA"
  env <- varDeclsToFunEnv s vds
  assert s (exprToSExpr env e)
  print =<< check s
  print =<< getExprs s (map snd env)

proveProgram :: Show t => Program t -> IO ()
proveProgram p =
    case assertionsOfProgram p of
        [] -> error "in proveProgram: at least one assertion required"
        a:_ -> do
            putStrLn "Launching SMT solver"
            proveExpr (globalsOfProgram p) (exprOfAssertion a)

proveProgramTest :: Show t => Program t -> IO ()
proveProgramTest p =  do
  l <- newLogger 0
  -- s <- newSolver "cvc4" ["--lang=smt2"] (Just l)
  s <- newSolver "z3" ["-in"] Nothing
  setLogic s "QF_LIA"
  x <- declare s "x" tInt
  assert s (add x (int 2) `eq` int 5)
  print =<< check s
  print =<< getExprs s [x]

