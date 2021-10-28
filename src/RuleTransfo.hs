
{-# LANGUAGE LambdaCase #-}
module RuleTransfo where

import L4.Annotation ( HasDefault(defaultVal))
import L4.KeyValueMap
    ( ValueKVM(..), KVMap, KeyKVM, hasPathMap, getAssocOfPathMap, stringListAsKVMap, putAssocOfPathMap )
import L4.Syntax
import SyntaxManipulation (appToFunArgs, funArgsToApp, conjExpr, conjsExpr, liftType, notExpr, disjsExpr, remapExpr, eqExpr, liftExpr, isLocalVar, fv, dnf, nnf )
import L4.Typing (distinct, eraseAnn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.List (sortBy)

import Data.Graph.Inductive.Graph
    ( nodes, Graph(labEdges, mkGraph), LNode, Node )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.Graph.Inductive.Query.DFS ( scc, topsort' )
import PrintProg (printARName)


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


-------------------------------------------------------------
-- Rule transformations
-------------------------------------------------------------

type DecompRule t = Rule t -> [Rule t]
type DecompWorklist t = [Rule t] -> [Rule t]
type RuleTransformer t = Rule t -> Rule t

-- Transforms: for x: Tx if A then all y: Ty. F into: for x: Tx, y: Ty if A then F
-- (one step, rule unchanged if transfo not applicable)
ruleAllR :: RuleTransformer (Tp ())
ruleAllR r =
  case postcondOfRule r of
    QuantifE _ All v e ->
      r{postcondOfRule = e}{precondOfRule = liftExpr 0 (precondOfRule r)}{varDeclsOfRule = varDeclsOfRule r ++ [v]}
    _ -> r

-- Transforms: if A then B --> C into: if A, B then C
-- (one step, rule unchanged if transfo not applicable)
ruleImplR :: RuleTransformer (Tp ())
ruleImplR r =
  case postcondOfRule r of
    BinOpE _ (BBool BBimpl) e1 e2 -> r{postcondOfRule = e2}{precondOfRule = conjExpr (precondOfRule r) e1}
    _ -> r

-- Transforms rule: if A then B && C into two rules: if A then B and: if A then C
ruleConjR :: DecompRule t
ruleConjR r =
  case postcondOfRule r of
    BinOpE t (BBool BBand) e1 e2 ->
      [ r{postcondOfRule = e1}{nameOfRule = modifyARName (nameOfRule r) "Conj1"}
      , r{postcondOfRule = e2}{nameOfRule = modifyARName (nameOfRule r) "Conj2"}]
    _ -> [r]

-- Transforms rule: if A1 || ... An then C into n rules: if A1 then C ... and if An then C
-- If the precondition is not explicitly a disjunction, it is converted into disjunctive normal form first
ruleDisjL :: DecompRule (Tp())
ruleDisjL r =
  let dnfDecomp = dnf (nnf True (precondOfRule r))
  in case dnfDecomp of
    [_] -> [r]     -- decomposition yields one disjunct --> return rule unchanged
    _ -> zipWith (\ cnjs i -> r {precondOfRule = conjsExpr cnjs}
                                {nameOfRule = modifyARName (nameOfRule r) ("Disj" ++ show i)})
                 dnfDecomp [0 .. length dnfDecomp - 1]


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
ruleAbstrInstances :: RuleTransformer (Tp ())
ruleAbstrInstances r =
  case postcondOfRule r of
    e@AppE {} ->
        let vds = varDeclsOfRule r
            (f, args) = appToFunArgs [] e
            (rargs, reqs, rds) = constrNewArgs (length vds) args
        in r{postcondOfRule = funArgsToApp f rargs}
            {precondOfRule = conjsExpr (precondOfRule r:reqs) }
            {varDeclsOfRule = rds ++ varDeclsOfRule r }
    _ -> r

localVarExpr :: Expr t -> Bool
localVarExpr (VarE _ (LocalVar _ _)) = True
localVarExpr _ = False

splitDecls :: Set.Set Int -> ([VarDecl t], [VarDecl t], [VarDecl t]) -> ([VarDecl t], [VarDecl t], [VarDecl t])
splitDecls fvs (lowers,this,[]) = (lowers,this,[])
splitDecls fvs (lowers,this,u:us) =
  if Set.member (length lowers) fvs
  then splitDecls fvs (lowers++[u],this,us)
  else (lowers,[u],us)

-- lifts an existential quantifier in the preconditions into the var decls of the rule
ruleExLStep :: RuleTransformer (Tp ())
ruleExLStep r =
  let vds = varDeclsOfRule r
      prec = precondOfRule r
      postc = postcondOfRule r
  in case prec of
    QuantifE _ Ex v e -> r{varDeclsOfRule = vds ++ [v]}{precondOfRule = e}{postcondOfRule = liftExpr 0 postc}
    _ -> r

ruleExL :: RuleTransformer (Tp ())
ruleExL = fixp ruleExLStep


-- ruleExLInvStep is the inverse of ruleExLStep:
-- It introduces the innermost declared variable that is not a free variable of the postcondition as 
-- existential variable of the precondition:
-- for x1 ... y ... xn:    pre(x1 ... y ... xn) --> post(x1 ... xn)
-- becomes
-- for x1 ... xn:   (exists y.  pre(x1 ... y ... xn)) --> post(x1 ... xn)
-- If there is no such variable, the rule is returned unchanged.
ruleExLInvStep :: RuleTransformer (Tp ())
ruleExLInvStep r =
  let vds = varDeclsOfRule r
      prec = precondOfRule r
      postc = postcondOfRule r
      fvids = Set.map indexOfVar (Set.filter isLocalVar (fv postc))
      (lowers, this, uppers) = splitDecls fvids ([],[],reverse vds)
  in case this of
    -- variable set has not been split because all vardecls are fvs of postcond
    -- rule not modified
    [] -> r
    -- variable set has been split
    [vd] ->
      let ll = length lowers
          lvds = length vds
          rmpPostc = zip [ll + 1 .. lvds - 1] [ll .. lvds - 2]
          rmpPrec  = (ll, 0) : zip [0 .. ll - 1] [1 .. ll]
          newPostc = remapExpr rmpPostc postc
          newPrec = QuantifE BooleanT Ex vd (remapExpr rmpPrec prec)
      in r{varDeclsOfRule = reverse (lowers++uppers)}{precondOfRule = newPrec}{postcondOfRule = newPostc}
    _ -> error "internal error in splitDecls: "

ruleExLInv :: RuleTransformer (Tp ())
ruleExLInv = fixp ruleExLInvStep

-- Condition of applicability: 
-- the rule has to be of the form for xi .. xj  if Pre( ... ) then f xm .. xk
-- where f is a function application and the variables xm .. xk are mutually distinct and are exactly the variables in xi .. xj.
-- The function produces a normalized representation such that the arguments of f are applied to indices in decreasing order:
-- for xn .. x0  if Pre( ... ) then f xn .. x0
ruleNormalizeVarOrder :: RuleTransformer (Tp ())
ruleNormalizeVarOrder r =
  case postcondOfRule r of
    postc@AppE {} ->
        let vds = varDeclsOfRule r
            (f, args) = appToFunArgs [] postc
        in
          if all localVarExpr args && distinct (map (\(VarE _ (LocalVar vn i)) -> i) args) && length vds == length args
          then
            let idcs = map (\(VarE _ (LocalVar vn i)) -> i) args  -- the sequence of indices of the argument vars
                varrmp = zip idcs  (reverse [0 .. length idcs -1])   -- remap of variables
                idcsdecls = zip (reverse [0 .. length idcs -1]) vds  -- current correspondence index -> vardecl
                idcsdeclsSorted = sortBy (flip (\(a,_) (b,_) -> compare (lookup a varrmp) (lookup b varrmp))) idcsdecls
                newPostc = remapExpr varrmp postc
                newPrec = remapExpr varrmp (precondOfRule r)
            in r{varDeclsOfRule = map snd idcsdeclsSorted}{precondOfRule = newPrec}{postcondOfRule = newPostc}
          else r
    _ -> r

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
  in Rule BooleanT rn [] (varDeclsOfRule r1) (postcondOfRule r1) (disjsExpr (map precondOfRule rls))


compatibleRuleSignature :: Eq t => Rule t -> Rule t -> Bool
compatibleRuleSignature r1 r2 =
  let tps1 = map tpOfVarDecl (varDeclsOfRule r1)
      tps2 = map tpOfVarDecl (varDeclsOfRule r2)
  in tps1 == tps2

-- Adds negated precondition of r1 to r2. Corresponds to:
-- - "r1 subject to r2" (as annotation of r1: r2 has precedence over r1)
-- - "r2 despite r1" synonymous to  "r1 subject to r2"
-- Both rules have to abstract over the same variables. 
-- If they are not right from the start, try to quasi-normalize them.
-- Full normalization is too much (e.g. for rules with postcond P(const1) and P(const2))
restrictWithNegPrecondOfStep :: Rule (Tp ()) -> Rule (Tp ()) -> Rule (Tp ())
restrictWithNegPrecondOfStep r1 r2 =
  if compatibleRuleSignature r1 r2
  then r1{precondOfRule = conjExpr (precondOfRule r1) (notExpr (precondOfRule r2))}
  else 
    let r1Norm = ruleExLInv r1
        r2Norm = ruleExLInv r2
    in if compatibleRuleSignature r1Norm r2Norm
       then r1Norm{precondOfRule = conjExpr (precondOfRule r1Norm) (notExpr (precondOfRule r2Norm))}
       else error ("trying to merge rules " ++ printARName (nameOfRule r1) ++ " and "
                ++ printARName (nameOfRule r2) ++ " with incompatible signatures")

restrictWithNegPrecondOf :: Rule (Tp ()) ->  [Rule (Tp())] -> Rule (Tp())
restrictWithNegPrecondOf = foldl restrictWithNegPrecondOfStep

liftDecompRule :: DecompRule t -> DecompWorklist t
liftDecompRule = concatMap

fixp :: Eq t => (t -> t) -> (t -> t)
fixp f a =
  let newa = f a in
    if newa == a
    then a
    else fixp f newa

clarify :: DecompWorklist (Tp ())
clarify = fixp (liftDecompRule (ruleConjR .  ruleImplR . ruleAllR))

normalize :: RuleTransformer (Tp ())
normalize = ruleNormalizeVarOrder . ruleAbstrInstances . ruleExLInv

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

definedKVM :: KVMap
definedKVM = [("defined", MapVM [])]

derivedKVMForSubjectTo :: KeyKVM -> [String] -> [(String, ValueKVM)]
derivedKVMForSubjectTo original args =
  [( "derived", MapVM [( "apply", MapVM (stringListAsKVMap ("restrictWithNegPrecondOf" : original : args)))])]


rewriteRuleSubjectTo :: Rule t -> [Rule t]
rewriteRuleSubjectTo rl =
  if hasPathMap ["restrict", "subjectTo"] (instrOfRule rl)
  then
    let origName = appendToARName (nameOfRule rl) "'Orig"
        rlOrig = rl{instrOfRule = definedKVM, nameOfRule = origName}
        rlDeriv = rl{instrOfRule = derivedKVMForSubjectTo (arNameToString origName) (getRuleNamesAt ["restrict", "subjectTo"] rl)}
    in [rlOrig, rlDeriv]
  else [rl]

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


derivedInstrToRuleProg :: KVMap -> RuleProg
derivedInstrToRuleProg [("derived", IdVM rn)] =  RuleName rn
derivedInstrToRuleProg [("derived", MapVM [kvp])] =  keyValPairToRuleProg kvp
derivedInstrToRuleProg _ = error "illegal format for derived instruction"

keyValPairToRuleProg :: (KeyKVM, ValueKVM) -> RuleProg
keyValPairToRuleProg (k,  MapVM []) = RuleName k
keyValPairToRuleProg ("apply", MapVM ((fn,MapVM []) : args)) = Apply fn (map keyValPairToRuleProg args)
keyValPairToRuleProg p = error ("illegal form of rule program " ++ show p)

ruleNamesInDerived :: Rule t -> Set.Set String
ruleNamesInDerived rl =
  if hasPathMap ["derived"] (instrOfRule rl)
  then ruleNamesOfRuleProg (derivedInstrToRuleProg (instrOfRule rl))
  else Set.empty


-- TODO: temporary, only for testing
printDerivs :: [Rule t] -> String
printDerivs rls =
  let derivs = filter (hasPathMap ["derived"] . instrOfRule) rls
      convs = map (derivedInstrToRuleProg . instrOfRule) derivs
      showconvs = map show convs
  in unlines showconvs


findRule :: [Rule t] -> ARName -> Rule t
findRule rls rn = fromMaybe (error ("findRule: rule with name " ++ arNameToString rn ++ " should be in list")) (lookup rn (map (\r -> (nameOfRule r, r)) rls))

-- rule functions convert rule lists to rule lists
findRuleFun :: String -> [Rule (Tp())] -> Rule (Tp())
findRuleFun "inversion" rls = rulesInversion rls
findRuleFun "normalize" [rl] = normalize rl
findRuleFun "normalize" _ = error "normalize rule function must have exactly one argument"
findRuleFun "restrictWithNegPrecondOf" (rl:rls) = restrictWithNegPrecondOf rl rls
findRuleFun "restrictWithNegPrecondOf" _ = error "restrictWithNegPrecondOf rule function must have at least one argument"
findRuleFun _ _ = error "undefined rule function"

runRuleProg :: RuleProg -> [Rule (Tp())] -> Rule (Tp())
runRuleProg (RuleName rn) rls = findRule rls (Just rn)
runRuleProg (Apply fn args) rls = findRuleFun fn (map (`runRuleProg` rls) args)


edgeListGraphToGrNodes :: [a] -> [LNode a]
edgeListGraphToGrNodes ns = zip [0 .. length ns -1] ns

-- the elements swapped as compared to the above
grToEdgeListGraphNodes :: [a] -> [(a, Node)]
grToEdgeListGraphNodes ns = zip ns [0 .. length ns -1]

-- list of rule pairs (r1, r2) such that r1 depends on r2, i.e. r2 occurs in the derivation of r1
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

-- rules are ordered in such a way that a rule depends on the elements following it in the list
-- but not on the elements preceding it
topsortRules :: [Rule (Tp())] -> [Rule (Tp())]
topsortRules rls =
  let (ruleGr, nodeToRlMap) = rulesToGraph rls
  in
    case cyclesInRuleGraph nodeToRlMap ruleGr of
      [] -> topsort' ruleGr
      c -> error ("mutual dependency among rules " ++ unlines (map unlines c))


rewriteRuleDerived :: [Rule (Tp())] -> RuleTransformer (Tp())
rewriteRuleDerived rls rl =
  if hasPathMap ["derived"] (instrOfRule rl)
  then
    let prg = derivedInstrToRuleProg (instrOfRule rl)
        derRl = runRuleProg prg rls
    in derRl {nameOfRule = nameOfRule rl, instrOfRule = instrOfRule rl}
  else rl

rewriteRuleSetDerived :: [Rule (Tp())] -> [Rule (Tp())]
rewriteRuleSetDerived rls =
  let rlsSorted = topsortRules rls
  -- in map (rewriteRuleDerived rls) rlsSorted
  --in rlsSorted
  in foldr (\rl accrls -> rewriteRuleDerived accrls rl : accrls) [] rlsSorted
