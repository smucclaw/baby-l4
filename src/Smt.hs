-- L4 to SMT interface using the SimpleSMT library

module Smt(proveProgram) where

import Syntax
import Annotation (TypeAnnot (getType), LocTypeAnnot (typeAnnot))
import Typing (getTypeOfExpr, isBooleanTp, isIntegerTp, isFloatTp, superClassesOfClassDecl, spine)

import SimpleSMT as SMT
import qualified Data.Maybe
import Control.Monad ( when, unless )
import Text.Pretty.Simple (pPrint, pPrintString)
import RuleTransfo ( prenexForm, ruleImplR, liftDecompRule, repeatDecomp, ruleAllR, clarify, ruleAbstrInstances, ruleExL, ruleExLInv )


-------------------------------------------------------------
-- Extensions to SExpr construction currently not in SimpleSMT
-------------------------------------------------------------

declareSort :: Solver -> String -> Int -> IO SExpr
declareSort proc srt ar =
  do ackCommand proc $ fun "declare-sort" [ Atom srt, Atom (show ar) ]
     return (SMT.const srt)

getModel  :: Solver -> IO SExpr --[(SExpr, Value)]
getModel proc =
  command proc $ List [ Atom "get-model" ]


-------------------------------------------------------------
-- Conversion functions from L4 Expr to SExpr
-------------------------------------------------------------

-- mapping class names to sorts
type SMTSortEnv = [(ClassName, SExpr)]
-- mapping variable names to their sorts
type SMTFunEnv = [(VarName, SExpr)]
data SMTEnv = SMTEnv { sortEnv :: SMTSortEnv,
                       funEnv  :: SMTFunEnv }

quantifToSMT :: Quantif -> String
quantifToSMT All = "forall"
quantifToSMT Ex = "exists"

quantif :: Quantif -> SExpr -> SExpr -> SExpr
quantif q vds e = fun (quantifToSMT q) [vds, e]

-- local variable reference in a quantified expression 
localVarRef :: VarName -> SExpr
localVarRef = Atom


-- TODO: distinction between ideal mathematical numbers (tInt, tReal)
-- vs their implementation (integer words, floats)
tpToSort :: SMTSortEnv -> Tp -> SExpr
tpToSort se t
  | isBooleanTp t = tBool
  | isIntegerTp t = tInt
  | isFloatTp t = tReal
  | otherwise = case t of
                  ClassT cn -> Data.Maybe.fromMaybe (error $ "internal error in tpToSort: Type not found: " ++ show cn) (lookup cn se)
                  _ -> error $ "in tpToSort: " ++ show t ++ " not supported"

tpToRank :: SMTSortEnv -> Tp -> ([SExpr], SExpr)
tpToRank se f@(FunT t1 t2) =
  let (args, res) = spine [] f
  in (map (tpToSort se) args, tpToSort se res)
tpToRank se t = ([], tpToSort se t)


-- local variable declaration in a quantification. 
-- TODO: only first-order quantification, no functional types
-- (has to be checked in advance)
varTypeToSExprTD :: SMTSortEnv -> VarName -> Tp -> SExpr
varTypeToSExprTD se vn t = List [List [Atom vn, snd (tpToRank se t)]]

-- SMT variable / function declaration
varDeclToFun :: Solver -> SMTSortEnv -> VarDecl t -> IO (VarName, SExpr)
varDeclToFun s se (VarDecl _ vn vt) =
  let (args, res) = tpToRank se vt
  in do
     se <- declareFun s vn args res
     return (vn, se)

varDeclsToFunEnv :: Solver -> SMTSortEnv -> [VarDecl t] -> IO SMTFunEnv
varDeclsToFunEnv s se = mapM (varDeclToFun s se)


classDeclsToSortEnv :: Solver -> [ClassDecl t] -> IO SMTSortEnv
classDeclsToSortEnv s cds = mapM (classDeclToSort s) [cd | cd <- cds,  ClsNm "Class" `elem` superClassesOfClassDecl cd ]

classDeclToSort :: Solver -> ClassDecl t -> IO (ClassName, SExpr)
classDeclToSort s (ClassDecl _ cn _) =
  do
    se <- declareSort s (stringOfClassName cn) 0
    return (cn, se)


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

sExprApply :: SExpr -> SExpr -> SExpr
sExprApply f a = case f of
  Atom _ -> List [f, a]
  List es -> List (es ++ [a])

exprToSExpr :: Show t => SMTEnv -> Expr t -> SExpr
exprToSExpr env (ValE _ v) = valToSExpr v
exprToSExpr env (VarE _ v) = varToSExpr (funEnv env) v
exprToSExpr env (UnaOpE _ u e) = transUnaOp u (exprToSExpr env e)
exprToSExpr env (BinOpE _ b e1 e2) = transBinOp b (exprToSExpr env e1) (exprToSExpr env e2)
exprToSExpr env (IfThenElseE _ c e1 e2) = ite (exprToSExpr env c) (exprToSExpr env e1) (exprToSExpr env e2)
exprToSExpr env (QuantifE _ q vn t e) =
  quantif q (varTypeToSExprTD (sortEnv env) vn t) (exprToSExpr env e)
exprToSExpr env (AppE _ f a) = sExprApply (exprToSExpr env f) (exprToSExpr env a)
exprToSExpr env e = error ("exprToSExpr: term " ++ show e ++ " not translatable")
-- TODO: still incomplete


-------------------------------------------------------------
-- Launching the solver and retrieving results
-------------------------------------------------------------

proveExpr :: Show t => [ClassDecl t] -> [VarDecl t] -> Expr t ->IO ()
proveExpr cds vds e = do
  l <- newLogger 0
  -- s <- newSolver "cvc4" ["--lang=smt2"] (Just l)
  s <- newSolver "z3" ["-in"] (Just l)
  setLogic s "LIA"
  sEnv <- classDeclsToSortEnv s cds
  fEnv <- varDeclsToFunEnv s sEnv vds
  assert s (exprToSExpr (SMTEnv sEnv fEnv) e)
  checkRes <- check s
  print checkRes
  when (checkRes == Sat) $ do
    pPrint =<< getModel s
-- print =<< check s
--  print =<< getExprs s (map snd fEnv)

proveExprTest  :: Show t => [ClassDecl t] -> [VarDecl t] -> Expr t ->IO ()
proveExprTest cds vds e =
  pPrint $ () <$ prenexForm e

proveProgramCorrect :: Show t => Program t -> IO ()
proveProgramCorrect p =
    case assertionsOfProgram p of
        [] -> error "in proveProgram: at least one assertion required"
        a:_ -> do
            putStrLn "Launching SMT solver"
            proveExpr (classDeclsOfProgram p) (globalsOfProgram p) (exprOfAssertion a)

-- this is only a test version, the definite version is proveProgramCorrect
proveProgram :: Program (LocTypeAnnot Tp) -> IO ()
proveProgram p =
  case rulesOfProgram p of
      [] -> error "in proveProgram: at least one rule required"
      r:_ -> do
        pPrint (ruleExL [fmap typeAnnot r])

