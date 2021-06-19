
{-# LANGUAGE LambdaCase #-}
module RuleTransfo where

import Annotation ( HasDefault(defaultVal) )
import KeyValueMap
    ( ValueKVM(..), KVMap, KeyKVM, selectAssocOfMap, hasPathValue, hasPathMap, getAssocOfPathMap, stringListAsKVMap, putAssocOfPathMap )
import Syntax
import Typing (appToFunArgs, funArgsToApp, distinct, eraseAnn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.List (sortBy)

import Data.Graph.Inductive.Graph
    ( nodes, Graph(labEdges, mkGraph), LNode, Node )
import Data.Graph.Inductive.PatriciaTree ( Gr ) 
import Data.Graph.Inductive.Query.DFS ( scc, topsort' )


----------------------------------------------------------------------
-- Elementary manipulation of rules and rule names
----------------------------------------------------------------------

exchangeARName:: ARName -> String -> ARName
exchangeARName Nothing rn = Nothing
exchangeARName (Just s) rn = Just rn

modifyARName:: ARName -> String -> ARName
modifyARName Nothing rn = Nothing
modifyARName (Just s) rn = Just (s ++ rn)

hasName :: ARName -> Bool
hasName Nothing = False
hasName _ = True

isNamedRule :: Rule t -> Bool
isNamedRule  = hasName . nameOfRule

----------------------------------------------------------------------
-- Logical infrastructure: macros for simplifying formula construction
----------------------------------------------------------------------

notExpr :: Expr (Tp ()) -> Expr (Tp())
notExpr = UnaOpE booleanT (UBool UBnot)

conjExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
conjExpr = BinOpE booleanT (BBool BBand)

disjExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
disjExpr = BinOpE booleanT (BBool BBor)

implExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
implExpr = BinOpE booleanT (BBool BBimpl)

conjsExpr :: [Expr (Tp ())] -> Expr (Tp ())
conjsExpr [] = trueV
conjsExpr [e] = e
conjsExpr (e:es) = conjExpr e (conjsExpr es)

disjsExpr :: [Expr (Tp ())] -> Expr (Tp ())
disjsExpr [] = falseV
disjsExpr [e] = e
disjsExpr (e:es) = disjExpr e (disjsExpr es)

eqExpr  :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
eqExpr = BinOpE booleanT (BCompar BCeq)

----------------------------------------------------------------------
-- Logical infrastructure: computing normal forms
----------------------------------------------------------------------

dropVarBy :: Int -> Var t -> Var t
dropVarBy n (LocalVar vn i) = LocalVar vn (i - n)
dropVarBy n v = v

localVarLowerThan :: Int -> Var t -> Bool
localVarLowerThan n (LocalVar vn i) = i <= n
localVarLowerThan n _ = True

-- Free variables of an expression. Also LocalVar count as free variables.
fv :: Ord t => Expr t -> Set.Set (Var t)
fv ValE {} = Set.empty
fv (VarE _ v) = Set.singleton v
fv (UnaOpE _  _ e) = fv e
fv (BinOpE _  _ e1 e2) = Set.union (fv e1) (fv e2)
fv (IfThenElseE _ c e1 e2) = Set.unions [fv c, fv e1, fv e2]
fv (AppE _ f a) = Set.union (fv f) (fv a)
fv (FunE _ p _ e) = Set.map (dropVarBy (patternLength p)) (Set.filter (localVarLowerThan (patternLength p - 1)) (fv e))
fv QuantifE {bodyOfExprQ = e} = Set.map (dropVarBy 1) (Set.filter (localVarLowerThan 0) (fv e))
fv (FldAccE _ e _) = fv e
fv (TupleE _ es) = Set.unions (map fv es)
fv (CastE _ _ e) = fv e
fv (ListE _ _ es) = Set.unions (map fv es)
fv (NotDeriv _ _ e) = fv e

-- Lift variables and expressions: 
-- Increase by one all indices of bound variables with index number >= n
-- Used for pushing an expression below a quantifier

liftVar :: Int -> Var t -> Var t
liftVar n (LocalVar vn i) = if n <= i then LocalVar vn (i + 1) else LocalVar vn i
liftVar n v = v

liftExpr :: Int -> Expr t -> Expr t
liftExpr n e@(ValE t v) = e
liftExpr n (VarE t v) = VarE t (liftVar n v)
liftExpr n (UnaOpE t u et) = UnaOpE t u (liftExpr n et)
liftExpr n (BinOpE t b et1 et2) = BinOpE t b (liftExpr n et1) (liftExpr n et2)
liftExpr n (IfThenElseE t et1 et2 et3) = IfThenElseE t (liftExpr n et1) (liftExpr n et2) (liftExpr n et3)
liftExpr n (AppE t et1 et2) = AppE t (liftExpr n et1) (liftExpr n et2)
liftExpr n (FunE t p ptp et) = FunE t p ptp (liftExpr (n + patternLength p) et)
liftExpr n (QuantifE t q vn vt et) = QuantifE t q vn vt (liftExpr (n+1) et)
liftExpr n (FldAccE t et f) = FldAccE t (liftExpr n et) f
liftExpr n (TupleE t ets) = TupleE t (map (liftExpr n) ets)
liftExpr n (CastE t tp et) = CastE t tp (liftExpr n et)
liftExpr n (ListE t lop ets) = ListE t lop (map (liftExpr n) ets)
liftExpr n (NotDeriv t b et) = NotDeriv t b (liftExpr n et)

-- Remap variables and expressions: 
-- Exchange indices of bound variables as indicated in the map
-- Used for permuting quantifiers
remapVar :: [(Int, Int)] -> Var t -> Var t
remapVar m (LocalVar vn i) = LocalVar vn (fromMaybe i (lookup i m))
remapVar _ v = v

remapExpr :: [(Int, Int)] -> Expr t -> Expr t
remapExpr m e@(ValE t v) = e
remapExpr m (VarE t v) = VarE t (remapVar m v)
remapExpr m (UnaOpE t u et) = UnaOpE t u (remapExpr m et)
remapExpr m (BinOpE t b et1 et2) = BinOpE t b (remapExpr m et1) (remapExpr m et2)
remapExpr m (IfThenElseE t et1 et2 et3) = IfThenElseE t (remapExpr m et1) (remapExpr m et2) (remapExpr m et3)
remapExpr m (AppE t et1 et2) = AppE t (remapExpr m et1) (remapExpr m et2)
remapExpr m (FunE t p ptp et) = FunE t p ptp (remapExpr (map (\(x, y) -> (x + patternLength p, y + patternLength p)) m) et)
remapExpr m (QuantifE t q vn vt et) = QuantifE t q vn vt (remapExpr (map (\(x, y) -> (x+1, y+1)) m) et)
remapExpr m (FldAccE t et f) = FldAccE t (remapExpr m et) f
remapExpr m (TupleE t ets) = TupleE t (map (remapExpr m) ets)
remapExpr m (CastE t tp et) = CastE t tp (remapExpr m et)
remapExpr m (ListE t lop ets) = ListE t lop (map (remapExpr m) ets)
remapExpr m (NotDeriv t b et) = NotDeriv t b (remapExpr m et)

swapQuantif :: Quantif -> Quantif
swapQuantif All = Ex
swapQuantif Ex  = All

prenexUnary :: t -> UBoolOp -> Expr t -> Expr t
prenexUnary t UBnot (QuantifE tq q vn vt et) = QuantifE tq (swapQuantif q) vn vt (UnaOpE t (UBool UBnot) et)
prenexUnary t u e = UnaOpE t (UBool u) e

prenexBinary :: t -> BBoolOp -> Expr t -> Expr t -> Expr t
prenexBinary t b e1 (QuantifE t2 q2 vn2 vt2 e2) = QuantifE t2 q2 vn2 vt2 (prenexBinary t b (liftExpr 0 e1) e2)
prenexBinary t b (QuantifE t1 q1 vn1 vt1 e1) e2 =
    let q = case b of
              BBimpl -> swapQuantif q1
              _ -> q1
    in QuantifE t1 q vn1 vt1 (prenexBinary t b e1 (liftExpr 0 e2))
prenexBinary t b e1 e2 = BinOpE t (BBool b) e1 e2

prenexForm :: Expr t -> Expr t
prenexForm (UnaOpE t (UBool u) et) = prenexUnary t u (prenexForm et)
prenexForm (BinOpE t (BBool b) et1 et2) = prenexBinary t b (prenexForm et1) (prenexForm et2)
prenexForm (QuantifE t q vn vt et) = QuantifE t q vn vt (prenexForm et)
prenexForm e = e


ruleToFormula :: Rule (Tp ()) -> Expr (Tp ())
ruleToFormula r = abstract All (varDeclsOfRule r) (implExpr (precondOfRule r) (postcondOfRule r))

abstract :: Quantif -> [VarDecl (Tp ())] -> Expr (Tp ()) -> Expr (Tp ())
abstract q vds e
  = foldr
      (\ vd -> QuantifE booleanT q (QVarName (annotOfVarDecl vd) (nameOfVarDecl vd)) (tpOfVarDecl vd)) e
      vds

-------------------------------------------------------------
-- Rule transformations
-------------------------------------------------------------

type DecompRule t = Rule t -> [Rule t]
type DecompWorklist t = [Rule t] -> [Rule t]

ruleAllR :: DecompRule (Tp ())
ruleAllR r =
  case postcondOfRule r of
    QuantifE _ All vn tp e ->
      [r{postcondOfRule = e}{precondOfRule = liftExpr 0 (precondOfRule r)}{varDeclsOfRule = varDeclsOfRule r ++ [VarDecl (eraseAnn  tp) (nameOfQVarName vn) tp]}]
    _ -> [r]

ruleImplR :: DecompRule (Tp ())
ruleImplR r =
  case postcondOfRule r of
    BinOpE _ (BBool BBimpl) e1 e2 -> [r{postcondOfRule = e2}{precondOfRule = conjExpr (precondOfRule r) e1}]
    _ -> [r]


ruleConjR :: DecompRule t
ruleConjR r =
  case postcondOfRule r of
    BinOpE t (BBool BBand) e1 e2 ->
      [ r{postcondOfRule = e1}{nameOfRule = modifyARName (nameOfRule r) "Conj1"}
      , r{postcondOfRule = e2}{nameOfRule = modifyARName (nameOfRule r) "Conj2"}]
    _ -> [r]

-- suggestion for new variable name, without guarantees that this name is unique
-- takes into account the variable's type and its rank
varNameSuggestion :: Tp () -> Int -> String
varNameSuggestion (ClassT () cn) n = "v" ++ stringOfClassName cn ++ "Gen" ++ show n
varNameSuggestion t n = "v" ++ "Gen" ++ show n

newArgCondition :: Expr t -> [Expr t] -> Bool
newArgCondition (VarE t (LocalVar vn i)) es =
  any (\case (VarE t (LocalVar vn2 i2)) -> i == i2
             _ -> False
    ) es
newArgCondition e es = True

liftType :: Tp () -> Tp (Tp ())
liftType t = KindT <$ t

constrNewArgs :: Int -> [Expr (Tp ())] -> ([Expr (Tp ())], [Expr (Tp ())], [VarDecl (Tp ())])
constrNewArgs n [] = ([], [], [])
constrNewArgs n (e:es) =
  if newArgCondition e es
  then
    let vn = varNameSuggestion (annotOfExpr e) n
        newvar = LocalVar (QVarName defaultVal vn) n
        tp = annotOfExpr e
        ve = VarE tp newvar
        (rargs, reqs, rds) = constrNewArgs (n+1) es
        nvd =  VarDecl tp vn (liftType tp)
    in (ve:rargs, eqExpr ve e:reqs, rds ++ [nvd])
  else let (rargs, reqs, rds) = constrNewArgs n es in (e:rargs, reqs, rds)

-- Abstract over instances: A rule of the form 
-- if Pre then R (.. e1 .. en ..)
-- is replaced by
-- if Pre && x1 = e1 && ... && xn = xn then R (.. x1 .. xn ..)
-- for new variables x1 .. xn. 
-- After abstraction, all arguments of R are distinct local variables. 
-- New variables are only introduced when necessary.
ruleAbstrInstances :: DecompRule (Tp ())
ruleAbstrInstances r =
  case postcondOfRule r of
    e@AppE {} ->
        let vds = varDeclsOfRule r
            (f, args) = appToFunArgs [] e
            (rargs, reqs, rds) = constrNewArgs (length vds) args
        in [r{postcondOfRule = funArgsToApp f rargs}
             {precondOfRule = conjsExpr (precondOfRule r:reqs) }
             {varDeclsOfRule = rds ++ varDeclsOfRule r }]
    _ -> [r]

localVarExpr :: Expr t -> Bool
localVarExpr (VarE _ (LocalVar _ _)) = True
localVarExpr _ = False

splitDecls :: Set.Set Int -> ([VarDecl t], [VarDecl t], [VarDecl t]) -> ([VarDecl t], [VarDecl t], [VarDecl t])
splitDecls fvs (lowers,this,[]) = (lowers,this,[])
splitDecls fvs (lowers,this,u:us) =
  if Set.member (length lowers) fvs
  then splitDecls fvs (lowers++[u],this,us)
  else (lowers,[u],us)

-- TODO: move elsewhere
isLocalVar :: Var t -> Bool
isLocalVar (LocalVar _ _) = True
isLocalVar _ = False

-- lifts an existential quantifier in the preconditions into the var decls of the rule
ruleExLStep :: DecompRule (Tp ())
ruleExLStep r =
  let vds = varDeclsOfRule r
      prec = precondOfRule r
      postc = postcondOfRule r
  in case prec of
    QuantifE _ Ex vn vt e ->
      let nvd = VarDecl (() <$ vt) (nameOfQVarName vn) vt
      in [r{varDeclsOfRule = vds ++ [nvd]}{precondOfRule = e}{postcondOfRule = liftExpr 0 postc}]
    _ -> [r]

ruleExL :: DecompWorklist (Tp ())
ruleExL = repeatDecomp (liftDecompRule ruleExLStep)


-- ruleExLInvStep is the inverse of ruleExLStep:
-- It introduces the innermost declared variable that is not a free variable of the postcondition as 
-- existential variable of the precondition:
-- for x1 ... y ... xn:    pre(x1 ... y ... xn) --> post(x1 ... xn)
-- becomes
-- for x1 ... xn:   (exists y.  pre(x1 ... y ... xn)) --> post(x1 ... xn)
-- If there is no such variable, the rule is returned unchanged.
ruleExLInvStep :: DecompRule (Tp ())
ruleExLInvStep r =
  let vds = varDeclsOfRule r
      prec = precondOfRule r
      postc = postcondOfRule r
      fvids = Set.map indexOfVar (Set.filter isLocalVar (fv postc))
      (lowers, this, uppers) = splitDecls fvids ([],[],reverse vds)
  in case this of
    -- variable set has not been split because all vardecls are fvs of postcond
    -- rule not modified
    [] -> [r]
    -- variable set has been split
    [vd] ->
      let ll = length lowers
          lvds = length vds
          rmpPostc = zip [ll + 1 .. lvds - 1] [ll .. lvds - 2]
          rmpPrec  = (ll, 0) : zip [0 .. ll - 1] [1 .. ll]
          newPostc = remapExpr rmpPostc postc
          newPrec = QuantifE booleanT Ex (QVarName (annotOfVarDecl vd) (nameOfVarDecl vd)) (tpOfVarDecl vd) (remapExpr rmpPrec prec)
      in [r{varDeclsOfRule = reverse (lowers++uppers)}{precondOfRule = newPrec}{postcondOfRule = newPostc}]
    _ -> error "internal error in splitDecls: "

ruleExLInv :: DecompWorklist (Tp ())
ruleExLInv = repeatDecomp (liftDecompRule ruleExLInvStep)


-- Condition of applicability: 
-- the rule has to be of the form for xi .. xj  if Pre( ... ) then f xm .. xk
-- where f is a function application and the variables xm .. xk are mutually distinct and are exactly the variables in xi .. xj.
-- The function produces a normalized representation such that the arguments of f are applied to indices in increasing order:
-- for xn .. x0  if Pre( ... ) then f x0 .. xn
ruleNormalizeVarOrder :: DecompRule (Tp ())
ruleNormalizeVarOrder r =
  case postcondOfRule r of
    postc@AppE {} ->
        let vds = varDeclsOfRule r
            (f, args) = appToFunArgs [] postc
        in
          if all localVarExpr args && distinct (map (\(VarE _ (LocalVar vn i)) -> i) args) && length vds == length args
          then
            let idcs = map (\(VarE _ (LocalVar vn i)) -> i) args
                idcsdecls = zip idcs vds
                varrmp = zip idcs [0 .. length idcs -1]
                idcsdeclsSorted = sortBy (\(a,_) (b,_) -> compare a b) idcsdecls
                newPostc = remapExpr varrmp postc
                newPrec = remapExpr varrmp (precondOfRule r)
            in [r{varDeclsOfRule = map snd idcsdeclsSorted}{precondOfRule = newPrec}{postcondOfRule = newPostc}]
          else [r]
    _ -> [r]


-- Inversion of a list of rules for xn .. x0 Pre1(x0 .. xn) -> P x0 .. xn , ... for xn .. xo Prem(x0 .. xn) -> P x0 .. xn
-- Condition of applicability: 
-- - list of rules is non-empty; 
-- - each of the rules is normalized 
-- TODO: instructions of generated rule set to []. More precise information?
rulesInversion :: [Rule (Tp())] -> Rule (Tp ())
rulesInversion rls =
  let r1 = head rls
      (VarE _ f, args) = appToFunArgs [] (postcondOfRule r1)
      rn = Just ((nameOfQVarName . nameOfVar) f ++ "Inversion")
  in Rule booleanT rn [] (varDeclsOfRule r1) (postcondOfRule r1) (disjsExpr (map precondOfRule rls))


-- Adds negated precondition of r1 to r2. Corresponds to:
-- - "r1 subject to r2" (as annotation of r1: r2 has precedence over r1)
-- - "r2 despite r1" synonymous to  "r1 subject to r2"
restrictWithNegPrecondOfStep :: Rule (Tp ()) -> Rule (Tp ()) -> Rule (Tp ())
restrictWithNegPrecondOfStep r1 r2 = r1{precondOfRule = conjExpr (precondOfRule r1) (notExpr (precondOfRule r2))}

restrictWithNegPrecondOf :: Rule (Tp ()) ->  DecompWorklist (Tp())
restrictWithNegPrecondOf rl rls = [foldl restrictWithNegPrecondOfStep rl rls]

liftDecompRule :: DecompRule t -> DecompWorklist t
liftDecompRule = concatMap

repeatDecomp :: Eq t => DecompWorklist t -> DecompWorklist t
repeatDecomp dec wl =
  let nwl = dec wl in
    if nwl == wl
    then wl
    else repeatDecomp dec nwl

clarify :: DecompWorklist (Tp ())
clarify = repeatDecomp (liftDecompRule ruleConjR . liftDecompRule ruleImplR . liftDecompRule ruleAllR)

normalize :: DecompWorklist (Tp ())
normalize = liftDecompRule ruleNormalizeVarOrder . liftDecompRule ruleAbstrInstances . ruleExLInv

-------------------------------------------------------------
-- Manipulating rule sets
-------------------------------------------------------------

-- Assumption: 
-- Three kinds of rules:
-- - defined
-- - derived (from other rules, without a reference to a "current" rule)
-- - restricted (current rule referencing other rules)
-- All the rules involved in the transformation have to be named. Possibly generate fake names first

---------------- rewrite "despite" restrictions ----------------
-- Assumption: despite-restrictions are written:
-- {restrict: {despite: rulename}}   or
-- {restrict: {despite: {rulename1, .. , rulenamen}}
-- Despite-restrictions may only reference rule names and not contain complex rule derivations
getRuleNamesAt :: [String] -> Rule t -> [String]
getRuleNamesAt pth rl =
  case getAssocOfPathMap pth (instrOfRule rl) of
    Just (IdVM s) -> [s]
    Just (MapVM mdesp) -> map fst mdesp
    _ -> []

addRestrictSubjectTo :: String -> Rule t -> Rule t
addRestrictSubjectTo nm rl =
  let oldRst = getRuleNamesAt ["restrict", "subjectTo"] rl
      newRst = nm : oldRst
      newInstr = putAssocOfPathMap ["restrict", "subjectTo"] (MapVM (stringListAsKVMap newRst)) (instrOfRule rl)
  in rl{instrOfRule=newInstr}

-- rewrite "restrict/despite" rule rl by adding "restrict/subjectTo" clauses in the referenced rules
rewriteRuleDespite :: Rule t -> [Rule t] -> [Rule t]
rewriteRuleDespite rl rls =
  let rds = map Just (getRuleNamesAt ["restrict", "despite"] rl)
      rlname = arNameToString (nameOfRule rl)
  in map (\arl -> if nameOfRule arl `elem` rds then addRestrictSubjectTo rlname arl else arl) rls

rewriteRuleSetDespite :: [Rule t] -> [Rule t]
rewriteRuleSetDespite rls =
  let restrictDespite = filter (hasPathMap ["restrict", "despite"] . instrOfRule) rls
  in foldr rewriteRuleDespite rls restrictDespite


---------------- rewrite "subjectTo" restrictions ----------------


-- TODO: move the following two to a section about rule name management 
appendToARName :: ARName -> String -> ARName
appendToARName Nothing _ = Nothing
appendToARName (Just n) s = Just (n ++ s)

arNameToString :: ARName  -> String
arNameToString = fromMaybe ""

rewriteRuleSubjectTo :: Rule t -> [Rule t]
rewriteRuleSubjectTo rl =
  if hasPathMap ["restrict", "subjectTo"] (instrOfRule rl)
  then
    let origName = appendToARName (nameOfRule rl) "'Orig"
        rlOrig = rl{instrOfRule = definedKVM, nameOfRule = origName}
        rlDeriv = rl{instrOfRule = derivedKVMForSubjectTo (arNameToString origName) (getRuleNamesAt ["restrict", "subjectTo"] rl)}
    in [rlOrig, rlDeriv]
  else [rl]

definedKVM :: KVMap
definedKVM = [("defined", MapVM [])]

derivedKVMForSubjectTo :: KeyKVM -> [String] -> [(String, ValueKVM)]
derivedKVMForSubjectTo original args =
  [( "derived", MapVM [( "apply", MapVM ([( "restrictWithNegPrecondOf", MapVM []), (original, MapVM [])] ++ stringListAsKVMap args))])]
{-
  [( "derived", MapVM [( "apply", MapVM [( "restrictWithNegPrecondOf", MapVM []),
                                  (original, MapVM []),
                                  ("list", MapVM (stringListAsKVMap args))])])]
-}

rewriteRuleSetSubjectTo :: [Rule t] -> [Rule t]
rewriteRuleSetSubjectTo = concatMap rewriteRuleSubjectTo



---------------- rewrite "derived" instructions ----------------

data RuleProg
  = RuleName String
  | Apply String [RuleProg]
  deriving (Eq, Ord, Show, Read)

ruleNamesOfRuleProg :: RuleProg -> Set.Set String
ruleNamesOfRuleProg (RuleName rn) = Set.singleton rn
ruleNamesOfRuleProg (Apply _ rps) = Set.unions (map ruleNamesOfRuleProg rps)


convertDerivedInstrToRuleProg :: KVMap -> RuleProg
convertDerivedInstrToRuleProg [("derived", IdVM rn)] =  RuleName rn
convertDerivedInstrToRuleProg [("derived", MapVM [kvp])] =  convertKeyValPairToRuleProg kvp
convertDerivedInstrToRuleProg _ = error "illegal format for derived instruction"

convertKeyValPairToRuleProg :: (KeyKVM, ValueKVM) -> RuleProg
convertKeyValPairToRuleProg (k,  MapVM []) = RuleName k
convertKeyValPairToRuleProg ("apply", MapVM ((fn,MapVM []) : args)) = Apply fn (map convertKeyValPairToRuleProg args)
convertKeyValPairToRuleProg p = error ("illegal form of rule program " ++ show p)

ruleNamesInDerived :: Rule t -> Set.Set String
ruleNamesInDerived rl =
  if hasPathMap ["derived"] (instrOfRule rl)
  then ruleNamesOfRuleProg (convertDerivedInstrToRuleProg (instrOfRule rl))
  else Set.empty


-- TODO: temporary, only for testing
printDerivs :: [Rule t] -> String
printDerivs rls =
  let derivs = filter (hasPathMap ["derived"] . instrOfRule) rls
      convs = map (convertDerivedInstrToRuleProg . instrOfRule) derivs
      showconvs = map show convs
  in unlines showconvs


findRule :: [Rule t] -> ARName -> Rule t
findRule rls rn = fromMaybe (error ("findRule: rule with name " ++ arNameToString rn ++ " should be in list")) (lookup rn (map (\r -> (nameOfRule r, r)) rls))

-- rule functions convert rule lists to rule lists
findRuleFun :: String -> DecompWorklist (Tp())
findRuleFun "restrictWithNegPrecondOf" (rl:rls) = restrictWithNegPrecondOf rl rls
findRuleFun "normalize" rls = normalize rls
findRuleFun "inversion" rls = [rulesInversion rls]
findRuleFun _ _ = error "undefined rule function"

runRuleProg :: RuleProg -> DecompWorklist (Tp())
runRuleProg (RuleName rn) rls = [findRule rls (Just rn)]
runRuleProg (Apply fn args) rls = findRuleFun fn (concatMap (`runRuleProg` rls) args)


edgeListGraphToGrNodes :: [a] -> [LNode a]
edgeListGraphToGrNodes ns = zip [0 .. length ns -1] ns

-- the elements swapped as compared to the above
grToEdgeListGraphNodes :: [a] -> [(a, Node)]
grToEdgeListGraphNodes ns = zip ns [0 .. length ns -1]

{-
edgeListGraphToGrEdges :: (a -> Node) -> [(a, a)] -> [LEdge ()]
edgeListGraphToGrEdges m = map (\(v1, v2) -> (m v1, m v2, ()))

-- conversion from edge-list format to Gr graph format
edgeListGraphToGr :: (Eq a) => EdgeListGraph a -> Gr a String
edgeListGraphToGr (ELG ns es) =
  let inv_nodes_map = grToEdgeListGraphNodes ns
      m = (\n -> fromJust (lookup n inv_nodes_map))
  in mkGraph (edgeListGraphToGrNodes ns) (edgeListGraphToGrEdges m es)
-}

derivedGraphEdgeRelation :: [Rule t] -> [(Rule t, Rule t)]
derivedGraphEdgeRelation rls =
  [(r1, r2) | r1 <- rls, r2 <- map (findRule rls . Just) (Set.toList (ruleNamesInDerived r1))]

rulesToGraph :: Eq t => [Rule t] -> (Gr (Rule t) (), Node -> Rule t)
rulesToGraph rls =
  let ndsRls = edgeListGraphToGrNodes rls
      rlsNds = grToEdgeListGraphNodes rls
      ndsRlsMap = (\n -> fromMaybe (error "in rulesToGraph: incorrect node lookup") (lookup n ndsRls))
      rlsNdsMap = (\r -> fromMaybe (error "in rulesToGraph: incorrect rule lookup") (lookup r rlsNds))
      rlsEdgs = derivedGraphEdgeRelation rls
      ndsEdgs = map (\(r1, r2) -> (rlsNdsMap r1, rlsNdsMap r2, ())) rlsEdgs
  in (mkGraph ndsRls ndsEdgs, ndsRlsMap)

isCyclic :: Gr a b -> Bool
isCyclic gr =  any (\(n1, n2, _) -> n1 == n2) (labEdges gr) || any (\c -> length c > 1) (scc gr)

cyclesInRuleGraph :: (Node -> Rule t) -> Gr (Rule t) () -> [[String]]
cyclesInRuleGraph ndsRlsMap gr =
  let cycNds = [[n] | n <- nodes gr, (n, n, ()) `elem` labEdges gr] ++ filter (\c -> length c > 1) (scc gr)
  in map (map (arNameToString . nameOfRule . ndsRlsMap)) cycNds



topsortRules :: [Rule (Tp())] -> [Rule (Tp())]
topsortRules rls =
  let (ruleGr, nodeToRlMap) = rulesToGraph rls
  in
    case cyclesInRuleGraph nodeToRlMap ruleGr of
      [] -> topsort' ruleGr
      c -> error ("mutual dependency among rules " ++ unlines (map unlines c))

    {-
    case [c | c <- stronglyConn, length c > 1] of
    --filter (\c -> length c > 1) stronglyConn of                -- 
      [] -> topsort' ruleGr
      c:cs -> error  "bla" -- ("mutual dependency among rules " + (map (\r -> arNameToString (nameOfRule r)) (map nodeToRlMap c)))
-}

rewriteRuleDerived :: [Rule (Tp())] -> Rule (Tp()) -> Rule (Tp())
rewriteRuleDerived rls rl =
  if hasPathMap ["derived"] (instrOfRule rl)
  then
    let prg = convertDerivedInstrToRuleProg (instrOfRule rl)
    in head (runRuleProg prg rls)
  else rl

rewriteRuleSetDerived :: [Rule (Tp())] -> [Rule (Tp())]
rewriteRuleSetDerived rls =
  let rlsSorted = topsortRules rls
  in map (rewriteRuleDerived rls) rlsSorted
