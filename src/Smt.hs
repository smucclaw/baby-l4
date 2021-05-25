-- L4 to SMT interface using the SimpleSMT library

module Smt(proveProgram) where

import Annotation (TypeAnnot (getType), LocTypeAnnot (typeAnnot))
import KeyValueMap
import Syntax
import Typing (getTypeOfExpr, isBooleanTp, isIntegerTp, isFloatTp, superClassesOfClassDecl, spine)

import SimpleSMT as SMT
import qualified Data.Maybe
import Control.Monad ( when, unless, foldM )
import Text.Pretty.Simple (pPrint, pPrintString)
import RuleTransfo ( prenexForm, ruleImplR, liftDecompRule, repeatDecomp, ruleAllR, clarify,
                    ruleAbstrInstances, ruleExL, ruleExLInv, ruleNormalizeVarOrder, rulesInversion, normalize,
                    ruleToFormula,
                    conjs, not )
import PrintProg (printRule)
import Data.Maybe (fromMaybe)
import Model (constructModel)


-------------------------------------------------------------
-- Extensions to SExpr construction currently not in SimpleSMT
-------------------------------------------------------------

declareSort :: Solver -> String -> Int -> IO SExpr
declareSort proc srt ar =
  do ackCommand proc $ fun "declare-sort" [ Atom srt, Atom (show ar) ]
     return (SMT.const srt)

getModel  :: Solver -> IO SExpr
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
transUArithOp UAminus = SMT.neg

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

-- Tried with the following provers:
-- alt-ergo gets stuck in interaction (apparently only reads from file)
-- Boolector (impossible to get compiled)
-- cvc4 does not work, apparently problems with quantifiers
-- mathsat terminates with an error (quantifiers not supported)
-- yices does not support logics like LIA
proveExpr :: Show t => Bool -> [ClassDecl t] -> [VarDecl t] -> Expr t ->IO ()
proveExpr checkSat cds vds e = do
  l <- newLogger 0
  --s <- newSolver "z3" ["-in"] (Just l)
  s <- newSolver "cvc4" ["--lang=smt2"] (Just l)
  setLogic s "AUFLIA"  -- for z3
  sEnv <- classDeclsToSortEnv s cds
  fEnv <- varDeclsToFunEnv s sEnv vds
  assert s (exprToSExpr (SMTEnv sEnv fEnv) e)
  checkRes <- check s
  print checkRes
  when (checkRes == Sat) $ do
    if checkSat
    then putStrLn "Formula satisfiable, found model."
    else putStrLn "Formula not valid, found countermodel."
    mdl <- getModel s
    pPrint mdl
    print (constructModel mdl)
  when (checkRes == Unsat) $ do
    if checkSat
    then putStrLn "Formula unsatisfiable."
    else putStrLn "Formula valid."
  when (checkRes == Unknown) $ do
    putStrLn "Solver produced unknown output."


-- TODO: to be defined in detail
defaultRuleSet :: Program t -> [Rule t]
defaultRuleSet = rulesOfProgram

-- TODO: rule specs are here supposed to be comma separated lists of rule names inclosed in { .. } 
-- It should also be possible to specify transformations to the rules 
rulesOfRuleSpec :: Program t -> ValueKVM -> [Rule t]
rulesOfRuleSpec p (MapVM kvm) =
  let nameRuleAssoc = map (\r -> (nameOfRule r, r)) (rulesOfProgram p)
  in map (\(k, v) -> fromMaybe (error ("rule name " ++ k ++ " unknown in rule set")) (lookup k nameRuleAssoc)) kvm
rulesOfRuleSpec p instr =
  error ("rule specification " ++ show instr ++ " should be a list (in { .. }) of rule names and transformations")

-- add rules of rs2 to rs1, not adding rules already existing in rs1 as determined by name
addToRuleSet :: [Rule t] -> [Rule t] -> [Rule t]
addToRuleSet rs1 rs2 = rs1 ++ [r2 | r2 <- rs2 ,  Prelude.not (any (\r1 -> nameOfRule r1 == nameOfRule r2) rs1) ]

-- delete from rs1 the rules in rs2 as determined by name
delFromRuleSet :: [Rule t] -> [Rule t] -> [Rule t]
delFromRuleSet rs1 rs2 = [r1 | r1 <- rs1 ,  Prelude.not (any (\r2 -> nameOfRule r1 == nameOfRule r2) rs2) ]


composeApplicableRuleSet :: Program t -> Maybe ValueKVM -> Maybe ValueKVM -> Maybe ValueKVM -> [Rule t]
composeApplicableRuleSet p mbadd mbdel mbonly =
  case mbonly of
    Just onlyRls -> addToRuleSet [] (rulesOfRuleSpec p onlyRls)
    Nothing -> addToRuleSet
                  (delFromRuleSet (defaultRuleSet p)
                                (rulesOfRuleSpec p (fromMaybe (MapVM []) mbdel)))
                  (rulesOfRuleSpec p (fromMaybe (MapVM []) mbadd))

selectApplicableRules :: Program t -> ValueKVM -> [Rule t]
selectApplicableRules p instr =
  case selectAssocVal "rules" instr of
    Nothing -> defaultRuleSet p
    Just rulespec -> composeApplicableRuleSet p
                      (selectAssocVal "add" rulespec)
                      (selectAssocVal "del" rulespec)
                      (selectAssocVal "only" rulespec)

proveAssertionSMT :: Program Tp -> ValueKVM -> Assertion Tp -> IO ()
proveAssertionSMT p instr asrt = do
  putStrLn "Launching SMT solver"
  let proveConsistency = selectOneOfInstr ["consistent", "valid"] instr == "consistent"
  let applicableRules = selectApplicableRules p instr
  let proofTarget = constrProofTarget proveConsistency asrt applicableRules
  proveExpr proveConsistency (classDeclsOfProgram p) (globalsOfProgram p) proofTarget


constrProofTarget :: Bool -> Assertion Tp -> [Rule Tp] -> Expr Tp
constrProofTarget sat asrt rls =
  let concl = exprOfAssertion asrt
      forms = map ruleToFormula rls
  in if sat
     then conjs (concl : forms)
     else conjs (RuleTransfo.not concl : forms)

-- TODO: details to be filled in
proveAssertionsCASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
proveAssertionsCASP p v asrt = putStrLn "No sCASP solver implemented"

proveAssertion :: Program Tp -> Assertion Tp -> IO ()
proveAssertion p asrt = foldM (\r (k,instr) ->
            case k of
              "SMT" -> proveAssertionSMT p instr asrt
              "sCASP"-> proveAssertionsCASP p instr asrt
              _ -> return ())
          () (instrOfAssertion asrt)

proveProgram :: Program (LocTypeAnnot Tp) -> IO ()
proveProgram p =
  let cleanedProg = fmap typeAnnot p
  in foldM (\r a -> proveAssertion cleanedProg a) () (assertionsOfProgram cleanedProg)


-- this is only a test version, the definite version is proveProgramCorrect
proveProgramTest :: Program (LocTypeAnnot Tp) -> IO ()
proveProgramTest p =
  case rulesOfProgram p of
      [] -> error "in proveProgram: at least one rule required"
      r1: r2: _ -> do
        putStrLn (printRule ((rulesInversion . normalize)  [fmap typeAnnot r1, fmap typeAnnot r2]))
        -- pPrint ((rulesInversion . normalize)  [fmap typeAnnot r1, fmap typeAnnot r2])

