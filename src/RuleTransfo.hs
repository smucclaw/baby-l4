
{-# LANGUAGE LambdaCase #-}
module RuleTransfo where

import Annotation ( HasDefault(defaultVal) )
import Syntax
import Typing (appToFunArgs, funArgsToApp, distinct, eraseAnn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.List (sortBy)


----------------------------------------------------------------------
-- Logical infrastructure: macros for simplifying formula construction
----------------------------------------------------------------------

not :: Expr (Tp ()) -> Expr (Tp())
not = UnaOpE booleanT (UBool UBnot)

conj :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
conj = BinOpE booleanT (BBool BBand)

disj :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
disj = BinOpE booleanT (BBool BBor)

impl :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
impl = BinOpE booleanT (BBool BBimpl)

conjs :: [Expr (Tp ())] -> Expr (Tp ())
conjs [] = trueV
conjs [e] = e
conjs (e:es) = conj e (conjs es)

disjs :: [Expr (Tp ())] -> Expr (Tp ())
disjs [] = falseV
disjs [e] = e
disjs (e:es) = disj e (disjs es)

eq  :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
eq = BinOpE booleanT (BCompar BCeq)

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
ruleToFormula r = abstract All (varDeclsOfRule r) (impl (precondOfRule r) (postcondOfRule r))

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
    BinOpE _ (BBool BBimpl) e1 e2 -> [r{postcondOfRule = e2}{precondOfRule = conj (precondOfRule r) e1}]
    _ -> [r]

ruleConjR :: DecompRule t
ruleConjR r =
  case postcondOfRule r of
    BinOpE t (BBool BBand) e1 e2 ->
      [ r{postcondOfRule = e1}{nameOfRule = nameOfRule r ++ "Conj1"}
      , r{postcondOfRule = e2}{nameOfRule = nameOfRule r ++ "Conj2"}]
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
    in (ve:rargs, eq ve e:reqs, rds ++ [nvd])
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
             {precondOfRule = conjs (precondOfRule r:reqs) }
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
rulesInversion :: [Rule (Tp())] -> Rule (Tp ())
rulesInversion rls =
  let r1 = head rls
      (VarE _ f, args) = appToFunArgs [] (postcondOfRule r1)
      rn = (nameOfQVarName . nameOfVar) f ++ "Inversion"
  in Rule booleanT rn (varDeclsOfRule r1) (postcondOfRule r1) (disjs (map precondOfRule rls))


-- Adds negated precondition of r1 to r2. Corresponds to:
-- - "r2 subject to r1" (as annotation of r2: r1 has precedence over r2)
-- - "r1 despite r2" (as annotation of r1: r1 has precedence over r2)
addNegPrecondTo :: Rule (Tp ()) -> Rule (Tp ()) -> Rule (Tp ())
addNegPrecondTo r1 r2 = r2{precondOfRule = conj (RuleTransfo.not (precondOfRule r1)) (precondOfRule r2)}


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
