-- L4 to SMT interface using the SimpleSMT library

module Smt(proveAssertionSMT, proveExpr, constrProofTarget) where

import L4.Annotation (LocTypeAnnot (typeAnnot))
import L4.KeyValueMap
    ( ValueKVM(MapVM, IntVM, IdVM),
      selectOneOfInstr,
      getAssocOfPathValue )
import L4.Syntax
import SyntaxManipulation (
      spine,
      ruleToFormula,
      conjsExpr,
      notExpr, etaExpand, decomposeFun)
import L4.Typing (isBooleanTp, isIntegerTp, isFloatTp, superClassesOfClassDecl)
import RuleTransfo
    ( isNamedRule,
      rewriteRuleSetDespite,
      rewriteRuleSetSubjectTo,
      rewriteRuleSetDerived )

import qualified SimpleSMT as SMT
import Control.Monad ( when, foldM )
import PrintProg (renameAndPrintRule, namesUsedInProgram, renameExpr, printARName )
import Data.Maybe (fromMaybe)
import Model (displayableModel, printDisplayableModel)
import qualified AutoAnnotations as SMT


-------------------------------------------------------------
-- Extensions to SExpr construction currently not in SimpleSMT
-------------------------------------------------------------

declareSort :: SMT.Solver -> String -> Int -> IO SMT.SExpr
declareSort proc srt ar =
  do SMT.ackCommand proc $ SMT.fun "declare-sort" [ SMT.Atom srt, SMT.Atom (show ar) ]
     return (SMT.const srt)

-- defineSort is here more restrictive than the SMT define-sort,
-- and is only used for aliasing sort symbols according to function sortAlias
defineSort :: SMT.Solver -> ClassName -> IO SMT.SExpr
defineSort proc cn =
  do SMT.ackCommand proc $ SMT.fun "define-sort" [ SMT.Atom (stringOfClassName cn), SMT.List [], sortAlias cn ]
     return (SMT.const (stringOfClassName cn))

getModel  :: SMT.Solver -> IO SMT.SExpr
getModel proc =
  SMT.command proc $ SMT.List [ SMT.Atom "get-model" ]


-------------------------------------------------------------
-- Conversion functions from L4 Expr to SExpr
-------------------------------------------------------------

-- mapping class names to sorts
type SMTSortEnv = [(ClassName, SMT.SExpr)]
-- mapping variable names to their sorts
type SMTFunEnv = [(VarName, SMT.SExpr)]
data SMTEnv = SMTEnv { sortEnv :: SMTSortEnv,
                       funEnv  :: SMTFunEnv }

quantifToSMT :: Quantif -> String
quantifToSMT All = "forall"
quantifToSMT Ex = "exists"

quantif :: Quantif -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
quantif q vds e = SMT.fun (quantifToSMT q) [vds, e]

-- local variable reference in a quantified expression 
localVarRef :: VarName -> SMT.SExpr
localVarRef = SMT.Atom


-- TODO: distinction between ideal mathematical numbers (tInt, tReal)
-- vs their implementation (integer words, floats)
tpToSort :: Show t => SMTSortEnv -> Tp t-> SMT.SExpr
tpToSort se t
  | isBooleanTp t = SMT.tBool
  | isIntegerTp t = SMT.tInt
  | isFloatTp t = SMT.tReal
  | otherwise = case t of
                  ClassT _ cn -> Data.Maybe.fromMaybe (error $ "internal error in tpToSort: Type not found: " ++ show cn) (lookup cn se)
                  _ -> error $ "in tpToSort: " ++ show t ++ " not supported"

tpToRank :: Show t => SMTSortEnv -> Tp t -> ([SMT.SExpr], SMT.SExpr)
tpToRank se f@(FunT _ t1 t2) =
  let (args, res) = spine [] f
  in (map (tpToSort se) args, tpToSort se res)
tpToRank se t = ([], tpToSort se t)


-- local variable declaration in a quantification. 
-- TODO: only first-order quantification, no functional types
-- (has to be checked in advance)
varTypeToSExprTD :: Show t => SMTSortEnv -> VarName -> Tp t -> SMT.SExpr
varTypeToSExprTD se vn t = SMT.List [SMT.List [SMT.Atom vn, snd (tpToRank se t)]]

-- SMT variable / function declaration
varDeclToFun :: Show t => SMT.Solver -> SMTSortEnv -> VarDecl t -> IO (VarName, SMT.SExpr)
varDeclToFun s se (VarDecl _ vn vt) =
  let (argTs, resT) = tpToRank se vt
  in do
     sf <- SMT.declareFun s vn argTs resT
     return (vn, sf)

predefinedToFun :: SMTFunEnv
predefinedToFun = [("distinct", SMT.Atom "distinct")]

varDeclsToFunEnv :: Show t => SMT.Solver -> SMTSortEnv -> [VarDecl t] -> IO SMTFunEnv
varDeclsToFunEnv s se vds =
  let predefEnv  = predefinedToFun
  in do
     funDeclEnv <- mapM (varDeclToFun s se) vds
     return (predefEnv ++ funDeclEnv)

varDefnToFun :: SMT.Solver -> SMTEnv -> VarDefn (Tp()) -> IO SMTEnv --(VarName, SMT.SExpr)
varDefnToFun s env (VarDefn _ vn vt e) =
  let (_, resT) = tpToRank (sortEnv env) vt
      (vds, bd) = decomposeFun (etaExpand (renameExpr [] e))
      params = map (\vd -> (nameOfVarDecl vd, tpToSort (sortEnv env) (tpOfVarDecl vd))) vds
      bdSE = exprToSExpr env bd
  in do
     sf <- SMT.defineFun s vn params resT bdSE
     return env{funEnv = funEnv env ++ [(vn, sf)]}

varDefnsToSMTEnv :: SMT.Solver -> SMTEnv -> [VarDefn (Tp())] -> IO SMTEnv
varDefnsToSMTEnv s = foldM (varDefnToFun s)

topLevelUserClassDecl :: ClassDecl t -> Bool
topLevelUserClassDecl cd = (length (superClassesOfClassDecl cd) >= 2) && superClassesOfClassDecl cd!!1 == ClassC

-- Only the following class declarations are meant to be translated to SMT sorts:
-- - Some special system defined classes (not subclasses of Class)
-- - Top-level user defined classes (direct subclasses of Class)
declarableSort :: ClassDecl t -> Bool
declarableSort cd =
  topLevelUserClassDecl cd ||
  nameOfClassDecl cd == StateC

-- For each definable sort, a sort alias has to be introduced in sortAlias
definableSort :: ClassDecl t -> Bool
definableSort cd =
  nameOfClassDecl cd == TimeC

sortAlias :: ClassName -> SMT.SExpr
sortAlias TimeC = SMT.Atom "Real"
sortAlias cn = error ("sort alias for " ++ show cn ++ " (internal error)")

classDeclToSortDecl :: SMT.Solver -> ClassDecl t -> IO (ClassName, SMT.SExpr)
classDeclToSortDecl s (ClassDecl _ cn _) =
  do
    se <- declareSort s (stringOfClassName cn) 0
    return (cn, se)

classDeclToSortDefn :: SMT.Solver -> ClassDecl t -> IO (ClassName, SMT.SExpr)
classDeclToSortDefn s (ClassDecl _ cn _) =
  do
    se <- defineSort s cn
    return (cn, se)


classDeclsToSortEnv :: SMT.Solver -> [ClassDecl t] -> IO SMTSortEnv
classDeclsToSortEnv s cds = do
  sdecls <- mapM (classDeclToSortDecl s) (filter declarableSort cds)
  sdefns <- mapM (classDeclToSortDefn s) (filter definableSort cds)
  return (sdecls ++ sdefns)


valToSExpr :: Val -> SMT.SExpr
valToSExpr (BoolV b) = SMT.bool b
valToSExpr (IntV i) = SMT.int i
valToSExpr (FloatV f) = SMT.real (toRational f)
valToSExpr _ = error "valToSExpr: not implemented"

-- TODO: For this to work, names (also of bound variables) have to be unique
varToSExpr :: SMTFunEnv -> Var t -> SMT.SExpr
varToSExpr env (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in Data.Maybe.fromMaybe
        (error $ "internal error in varToSExpr: Var not found: " ++ show vn)
        (lookup vn env)
varToSExpr env (LocalVar qvn i) = let vn = nameOfQVarName qvn in localVarRef vn

transUArithOp :: UArithOp ->  SMT.SExpr -> SMT.SExpr
transUArithOp UAminus = SMT.neg

transUBoolOp :: UBoolOp ->  SMT.SExpr -> SMT.SExpr
transUBoolOp UBnot = SMT.not

transUnaOp :: UnaOp -> SMT.SExpr -> SMT.SExpr
transUnaOp (UArith ua) = transUArithOp ua
transUnaOp (UBool ub) = transUBoolOp ub

transBArithOp :: BArithOp -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
transBArithOp BAadd = SMT.add
transBArithOp BAsub = SMT.sub
transBArithOp BAmul = SMT.mul
transBArithOp BAdiv = SMT.div
transBArithOp BAmod = SMT.mod

transBComparOp :: BComparOp -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
transBComparOp  BCeq = SMT.eq
transBComparOp  BClt = SMT.lt
transBComparOp  BClte = SMT.leq
transBComparOp  BCgt = SMT.gt
transBComparOp  BCgte = SMT.geq
transBComparOp  BCne = \e1 e2 -> SMT.not (SMT.eq e1 e2)

transBBoolOp :: BBoolOp -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
transBBoolOp BBimpl = SMT.implies
transBBoolOp BBor = SMT.or
transBBoolOp BBand = SMT.and

transBinOp :: BinOp -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
transBinOp (BArith ba) = transBArithOp ba
transBinOp (BCompar bc) = transBComparOp bc
transBinOp (BBool bb) = transBBoolOp bb

sExprApply :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
sExprApply f a = case f of
  SMT.Atom _ -> SMT.List [f, a]
  SMT.List es -> SMT.List (es ++ [a])

-- Important: Consistent references in Expr are maintained via de Bruijn indices, which do not exist in SExpr
-- Therefore, calls of exprToSExpr should be preceded by a call to renameExpr
exprToSExpr :: Show t => SMTEnv -> Expr t -> SMT.SExpr
exprToSExpr _   (ValE _ v) = valToSExpr v
exprToSExpr env (VarE _ v) = varToSExpr (funEnv env) v
exprToSExpr env (UnaOpE _ u e) = transUnaOp u (exprToSExpr env e)
exprToSExpr env (BinOpE _ b e1 e2) = transBinOp b (exprToSExpr env e1) (exprToSExpr env e2)
exprToSExpr env (IfThenElseE _ c e1 e2) = SMT.ite (exprToSExpr env c) (exprToSExpr env e1) (exprToSExpr env e2)
exprToSExpr env (QuantifE _ q v e) =
  quantif q (varTypeToSExprTD (sortEnv env) (nameOfVarDecl v) (tpOfVarDecl v)) (exprToSExpr env e)
exprToSExpr env (AppE _ f a) = sExprApply (exprToSExpr env f) (exprToSExpr env a)
exprToSExpr _    e = error ("exprToSExpr: term " ++ show e ++ " not translatable")
-- TODO: still incomplete


-------------------------------------------------------------
-- Launching the solver and retrieving results
-------------------------------------------------------------

-- When no explicit log level is provided, the default is set to 1
-- (silent mode not showing interaction with SMT solver)
selectLogLevel :: Maybe ValueKVM -> Int
selectLogLevel config =
  let defaultLogLevel = 1
  in case getAssocOfPathValue ["loglevel"] (fromMaybe (IntVM 0) config) of
    Nothing -> defaultLogLevel
    Just (IntVM n) -> fromIntegral n
    Just _ -> defaultLogLevel

createSolver :: Maybe ValueKVM -> Maybe SMT.Logger -> IO SMT.Solver
createSolver config lg =
  let defaultConfig = ("z3", ["-in"])
      (solverName, solverParams) = case config of
                                      Nothing -> defaultConfig
                                      Just vkvm -> case getAssocOfPathValue ["solver"] vkvm of
                                                      Just (IdVM "cvc4") -> ("cvc4", ["--lang=smt2"])
                                                      Just (IdVM "mathsat") -> ("mathsat", [])
                                                      _ -> defaultConfig
  in SMT.newSolver solverName solverParams lg


-- Tried with the following provers:
-- alt-ergo gets stuck in interaction (apparently only reads from file)
-- Boolector (impossible to get compiled)
-- cvc4 does not work with quantifiers, simple boolean or arithmetic queries supported
-- mathsat terminates with an error (quantifiers not supported), simple boolean or arithmetic queries supported
-- yices does not support logics like LIA
selectLogic :: SMT.Solver -> Maybe ValueKVM -> IO ()
selectLogic s config =
  let defaultConfig = "LIA"
      logicName = case config of
                     Nothing -> defaultConfig
                     Just vkvm -> case getAssocOfPathValue ["logic"] vkvm of
                                      Just (IdVM l) -> l
                                      _ -> defaultConfig
  in SMT.setLogic s logicName


-- Prove an expression e
-- For checkSat == True, check for satisfiability, otherwise for unsatisfiability
-- The expression e is assumed to be closed (no unbound local variables), 
-- but may contain references to global declared or defined variables as in vardecls / vardefns

proveExpr :: Maybe ValueKVM -> Bool -> [ClassDecl (Tp())] -> [VarDecl (Tp())] -> [VarDefn (Tp())] -> Expr (Tp()) ->IO ()
proveExpr config checkSat cdecls vardecls vardefns e = do
  l <- SMT.newLogger (selectLogLevel config)
  s <- createSolver config (Just l)
  selectLogic s config
  sEnv <- classDeclsToSortEnv s cdecls
  fEnv <- varDeclsToFunEnv s sEnv vardecls
  varDefnEnv <- varDefnsToSMTEnv s (SMTEnv sEnv fEnv) vardefns
  SMT.assert s (exprToSExpr varDefnEnv (renameExpr [] e))
  checkRes <- SMT.check s
  when (checkRes == SMT.Sat) $ do
    if checkSat
    then putStrLn "Formula satisfiable, found model."
    else putStrLn "Formula not valid, found countermodel."
    mdl <- getModel s
    -- pPrint mdl
    putStrLn (printDisplayableModel (displayableModel mdl))
  when (checkRes == SMT.Unsat) $ do
    if checkSat
    then putStrLn "Formula unsatisfiable."
    else putStrLn "Formula valid."
  when (checkRes == SMT.Unknown) $ do
    putStrLn "Solver produced unknown output."
  putStrLn ""


-- TODO: to be defined in detail
defaultRuleSet :: Program t -> [Rule t]
defaultRuleSet = rulesOfProgram

-- TODO: rule specs are here supposed to be comma separated lists of rule names inclosed in { .. } 
-- It should also be possible to specify transformations to the rules 
rulesOfRuleSpec :: Program t -> ValueKVM -> [Rule t]
rulesOfRuleSpec p (MapVM kvm) =
  let nameRuleAssoc = map (\r -> (fromMaybe "" (nameOfRule r), r)) (filter isNamedRule (rulesOfProgram p))
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
  case getAssocOfPathValue ["rules"] instr of
    Nothing -> defaultRuleSet p
    Just rulespec -> composeApplicableRuleSet p
                      (getAssocOfPathValue ["add"] rulespec)
                      (getAssocOfPathValue ["del"] rulespec)
                      (getAssocOfPathValue ["only"] rulespec)

proveAssertionSMT :: Program (Tp ()) -> ValueKVM -> Assertion (Tp ()) -> IO ()
proveAssertionSMT prg instr asrt = do
  putStrLn ("Launching SMT solver on " ++ printARName (nameOfAssertion asrt))
  let proveConsistency = selectOneOfInstr ["consistent", "valid"] instr == "consistent"
  let applicableRules = selectApplicableRules prg instr
  let proofTarget = constrProofTarget proveConsistency (map ruleToFormula applicableRules) (exprOfAssertion asrt) 
  let config = getAssocOfPathValue ["config"] instr
  proveExpr config proveConsistency (classDeclsOfProgram prg) (globalsOfProgram prg) [] proofTarget


constrProofTarget :: Bool -> [Expr (Tp())] -> Expr (Tp ()) -> Expr (Tp ())
constrProofTarget sat preconds concl =
  if sat
  then conjsExpr (concl : preconds)
  else conjsExpr (notExpr concl : preconds)

{-
proveProgramTest :: Program (LocTypeAnnot (Tp ())) -> IO ()
proveProgramTest p =
  do
    putStrLn "First transfo: rewrite Despite and SubjectTo"
    putStrLn (concatMap (renameAndPrintRule (namesUsedInProgram p)) (rewriteRuleSetSubjectTo (rewriteRuleSetDespite (rulesOfProgram (fmap typeAnnot p)))))
    putStrLn "Second transfo: rewrite Derived"
    putStrLn (concatMap (renameAndPrintRule (namesUsedInProgram p)) (rewriteRuleSetDerived (rewriteRuleSetSubjectTo (rewriteRuleSetDespite (rulesOfProgram (fmap typeAnnot p))))))
 -- putStrLn (printDerivs (rewriteRuleSetSubjectTo (rewriteRuleSetDespite (rulesOfProgram p))))
 -}