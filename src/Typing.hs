-- Typing of expressions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Typing where

import Data.List
import Data.Maybe

import Annotation
import Syntax


----------------------------------------------------------------------
-- Result of typing
----------------------------------------------------------------------


data AnnotTypingPhase
  = PosAnnotTP SRng
  | PosTpAnnotTP (LocTypeAnnot Tp)
  | PosClassHierAnnotTP (LocTypeAnnot [ClassName])
  deriving (Eq, Ord, Show, Read)


----------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------

-- Typing is done in an environment, composed of
-- the class decls, global and local variable declarations
type VarEnvironment = [(VarName, Tp)]

data Environment t = Env { classDeclsOfEnv :: [ClassDecl t]
                         , globalsOfEnv :: VarEnvironment
                         , localsOfEnv :: VarEnvironment }
  deriving (Eq, Ord, Show, Read)

-- prg is the current program to be typed
initialEnvOfProgram :: [ClassDecl t] -> [VarDecl t] -> Environment t
initialEnvOfProgram cds gvars =
  let initialGvs = map (\(VarDecl _ vn t) -> (vn, t)) gvars
  in Env cds initialGvs []


----------------------------------------------------------------------
-- Class manipulation
----------------------------------------------------------------------

classDefAssoc :: [ClassDecl t] -> [(ClassName, ClassDef t)]
classDefAssoc = map (\(ClassDecl _ cn cdf) -> (cn, cdf))

fieldAssoc ::  [ClassDecl t] -> [(ClassName, [FieldDecl t])]
fieldAssoc = map (\(ClassDecl _ cn cdf) -> (cn, fieldsOfClassDef cdf))


-- For a class name 'cn', returns the list of the names of the (non-strict) superclasses of 'cn'
-- Here, 'cdf_assoc' is an association of class names and class defs as contained in a program.
-- 'visited' is the list of class names already visited on the way up the class hierarchy
superClasses :: [(ClassName, ClassDef t)] -> [ClassName] -> ClassName -> [ClassName]
superClasses cdf_assoc visited cn =
  case lookup cn cdf_assoc of
    -- the following should not happen if definedSuperclass is true in a module
    Nothing -> error "in superClasses: cn not in cdf_assoc (internal error)"
    -- reached the top of the hierarchy
    Just (ClassDef [] _) -> reverse (cn : visited)
    -- class has super-class with name scn
    Just (ClassDef [scn] _) ->
      if scn `elem` visited
      then error ("cyclic superclass hierarchy for class " ++ (case cn of (ClsNm n) -> n))
      else superClasses cdf_assoc (cn : visited) scn
    Just (ClassDef _ _) -> error "in superClasses: superclass list should be empty or singleton (internal error)"

-- For each of a list of class declarations, returns its list of superclass names
-- TODO: not used anywhere
superClassesDecls :: [ClassDecl t] -> [[ClassName]]
superClassesDecls cds =
  let cdf_assoc = classDefAssoc cds
  in map (superClasses cdf_assoc [] . fst) cdf_assoc

-- TODO: not needed right now
--checkFieldDecl :: FieldDecl SRng -> FieldDecl AnnotTypingPhase
--checkFieldDecl (FieldDecl annot fn t) =  FieldDecl (PosAnnotTP annot) fn t

elaborateSupersInClassDecl :: (ClassName -> [ClassName]) -> ClassDecl t -> ClassDecl t
elaborateSupersInClassDecl supers (ClassDecl annot cn (ClassDef _ fds)) =
  ClassDecl annot cn (ClassDef (supers cn) fds)

-- in a class declaration, replace the reference to the immediate super-class 
-- by the list of all super-classes (from more to less specific, excluding the current class from the list)
elaborateSupersInClassDecls :: [ClassDecl t] -> [ClassDecl t]
elaborateSupersInClassDecls cds =
  let cdf_assoc = classDefAssoc cds
  in map (elaborateSupersInClassDecl (superClasses cdf_assoc [])) cds


localFields :: [(ClassName, [FieldDecl t])] -> ClassName -> [FieldDecl t]
localFields fd_assoc cn =
  fromMaybe [] (lookup cn fd_assoc)

-- in a class declaration, replace the list of local fields of the class by the list of all fields (local and inherited)
elaborateFieldsInClassDecls :: [ClassDecl t] -> [ClassDecl t]
elaborateFieldsInClassDecls cds =
  let fd_assoc = fieldAssoc cds
  in map (\(ClassDecl annot cn (ClassDef scs locfds)) ->
            ClassDecl annot cn (ClassDef scs (concatMap (localFields fd_assoc) scs))) cds


-- the class decl does not reference an undefined superclass
definedSuperclass :: [ClassName] -> ClassDecl t -> Bool
definedSuperclass cns cdc =
  case cdc of
    (ClassDecl _ cn (ClassDef [] _)) -> True
    (ClassDecl _ cn (ClassDef [scn] _)) ->
      elem scn cns || error ("undefined superclass for class " ++ (case cn of (ClsNm n) -> n))
    (ClassDecl _ cn (ClassDef _ _)) -> error "in definedSuperclass: superclass list should be empty or singleton (internal error)"

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

wellformedClassDecls :: [ClassDecl t] -> Bool
wellformedClassDecls cds =
  let class_names = map nameOfClassDecl cds
  in all (definedSuperclass class_names) cds && not (hasDuplicates class_names)

-- TODO: still check that field decls only reference declared classes
-- TODO: hasDuplicates should check that field names are unique 
--       and not only that (field name, type) is unique
wellFormedFieldDecls :: (Ord t) => ClassDecl t -> Bool
wellFormedFieldDecls (ClassDecl _ cn cdf) = not (hasDuplicates (fieldsOfClassDef cdf))

-- TODO: a bit of a hack. Error detection and treatment to be improved
-- - no ref to undefined superclass
-- - no cyclic graph hierarchy (implemented in superClasses above)
-- - no duplicate field declarations (local and inherited)
elaborateClsProgram :: (Ord t, Show t) => [ClassDecl t] -> [ClassDecl t]
elaborateClsProgram cds =
  if wellformedClassDecls cds
  then
    let ecdcs = elaborateFieldsInClassDecls (elaborateSupersInClassDecls cds)
    in
      if all wellFormedFieldDecls ecdcs
      then ecdcs
      else error ("Problem in field declarations: duplicate field declarations" ++ (show ecdcs))
  else error "Problem in class declarations"

-- TODO: currently INCORRECT, correct version in comment
strictSuperclassesOf :: Environment t -> ClassName -> [ClassName]
strictSuperclassesOf env cn = case lookup cn (classDefAssoc (classDeclsOfEnv env)) of
  Nothing -> error ("in strictSuperclassesOf: undefined class " ++ (case cn of (ClsNm n) -> n))
  -- Just (ClassDef supcls _) -> supcls
  Just (ClassDef supcls _) -> [cn]

superclassesOf :: Environment t -> ClassName -> [ClassName]
superclassesOf env cn = cn : strictSuperclassesOf env cn

isStrictSubclassOf :: Environment t -> ClassName -> ClassName -> Bool
isStrictSubclassOf env subcl supercl = supercl `elem` strictSuperclassesOf env subcl

isSubclassOf :: Environment t -> ClassName -> ClassName -> Bool
isSubclassOf env subcl supercl = supercl `elem` superclassesOf env subcl

-- TODO: currently INCORRECT, correct version in comment
fieldsOf :: Environment t -> ClassName -> [FieldDecl t]
fieldsOf env cn = case lookup cn (classDefAssoc (classDeclsOfEnv env)) of
  Nothing -> error ("in fieldsOf: undefined class " ++ (case cn of (ClsNm n) -> n))
  --Just (ClassDef _ fds) -> fds
  Just (ClassDef _ fds) -> []

longestCommonPrefix :: Eq a=> [a] -> [a] -> [a]
longestCommonPrefix (x:xs) (y:ys) = if x == y then x:longestCommonPrefix xs ys else []
longestCommonPrefix _ _ = []

-- least common superclass of two classes (given by their name) 
-- that must at least have Object as common superclass
leastCommonSuperClass :: Environment t -> ClassName -> ClassName -> ClassName
leastCommonSuperClass env cn1 cn2 =
  last (longestCommonPrefix (reverse (superclassesOf env cn1)) (reverse (superclassesOf env cn2)))

leastCommonSuperType :: Environment t -> Tp -> Tp -> Tp
leastCommonSuperType env (ClassT cn1) (ClassT cn2) = ClassT (leastCommonSuperClass env cn1 cn2)
leastCommonSuperType _ _ _ = error "internal errror: leastCommonSuperType should only be called on class types"

-- find all class definitions for class with name cn in environment
-- (assuming possibly duplicate definitions which would be an error)
-- TODO: this function is redundant with the functions above
-- Remove after error processing of classes and expressions is clearer
lookupClassDefInEnv :: Environment t -> ClassName -> [ClassDef t]
lookupClassDefInEnv env cn =
  map defOfClassDecl (filter (\cd -> nameOfClassDecl cd == cn) (classDeclsOfEnv env))


----------------------------------------------------------------------
-- Linking classes from the prelude to internal predicates
----------------------------------------------------------------------

booleanT = ClassT (ClsNm "Boolean")
integerT = ClassT (ClsNm "Integer")

stringT = ClassT (ClsNm "String")

isBooleanTp :: Tp -> Bool
isBooleanTp BoolT = True     -- TODO: BoolT currently still tolerated, but to be phased out
isBooleanTp (ClassT (ClsNm "Boolean")) = True
isBooleanTp _ = False

-- Note: IntT not taken as number type
isNumberTp :: Environment t -> Tp -> Bool
isNumberTp env (ClassT t) = isSubclassOf env t (ClsNm "Number")
isNumberTp _ _ = False

isScalarTp :: Tp -> Bool
isScalarTp BoolT = True
isScalarTp IntT = True
isScalarTp (ClassT _) = True
isScalarTp (FunT _ _) = False
isScalarTp (TupleT ts) = all isScalarTp ts
isScalarTp ErrT = True

isErrTp :: Tp -> Bool
isErrTp ErrT = True
isErrTp _ = False

isClassTp :: Tp -> Bool
isClassTp (ClassT _) = True
isClassTp _ = False


----------------------------------------------------------------------
-- Typing functions
----------------------------------------------------------------------


tpConstval :: Environment t -> Val -> Tp
tpConstval env x = case x of
  BoolV _ -> booleanT
  IntV _ -> integerT
  StringV _ -> stringT
  -- for record values to be well-typed, the fields have to correspond exactly (incl. order of fields) to the class fields.
  -- TODO: maybe relax some of these conditions.
  RecordV cn fnvals ->
    -- list of: field name, type of value
    let tfnvals = map (\(fn, v) -> (fn, (tpConstval env v))) fnvals
    in case lookupClassDefInEnv env cn of
       [] -> error ("class name " ++ (case cn of (ClsNm n) -> n) ++ " not defined")
       [cd] ->
         if map (\(FieldDecl _ fn t) -> (fn, t)) (fieldsOfClassDef cd) == tfnvals
         then ClassT cn
         else error ("record fields do not correspond to fields of class " ++ (case cn of (ClsNm n) -> n))
       _ -> error "internal error: duplicate class definition"
  ErrV -> ErrT

tpUarith :: Environment t -> Tp -> UArithOp -> Tp
tpUarith env t ua = if isNumberTp env t then t else ErrT

tpUbool :: Environment t -> Tp -> UBoolOp -> Tp
tpUbool env t ub = if isBooleanTp t then t else ErrT

tpUnaop :: Environment t -> Tp -> UnaOp -> Tp
tpUnaop env t uop = case uop of
  UArith ua  -> tpUarith env t ua
  UBool ub   -> tpUbool env t ub


tpBarith :: Environment t -> Tp -> Tp -> BArithOp -> Tp
tpBarith env t1 t2 ba =
  if isNumberTp env t1 && isNumberTp env t2
    then leastCommonSuperType env t1 t2
    else ErrT

-- TODO: more liberal condition for comparison?
tpBcompar :: Environment t -> Tp -> Tp -> BComparOp -> Tp
tpBcompar env t1 t2 bc =
  if isScalarTp t1 && isScalarTp t2
  then
    if (compatibleType env t1 t2 || compatibleType env t2 t1)
    then booleanT
    else ErrT
  else ErrT

tpBbool :: Environment t -> Tp -> Tp -> BBoolOp -> Tp
tpBbool env t1 t2 bc =
  if isBooleanTp t1 && isBooleanTp t2 then booleanT else ErrT

tpBinop :: Environment t -> Tp -> Tp -> BinOp -> Tp
tpBinop env t1 t2 bop = case bop of
  BArith ba  -> tpBarith env t1 t2 ba
  BCompar bc -> tpBcompar env t1 t2 bc
  BBool bb   -> tpBbool env t1 t2 bb


-- the first type can be cast to the second type
-- TODO: still to be defined
castCompatible :: Tp -> Tp -> Bool
castCompatible te ctp = True


-- typing of a variable that is initially (after parsing) only known by its name
tpVar :: Environment t -> Var -> Tp
tpVar env (GlobalVar vn) =
  case lookup vn (globalsOfEnv env) of
    Nothing -> Data.Maybe.fromMaybe ErrT (lookup vn (localsOfEnv env))
    Just t -> t
tpVar env (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

varIdentityInEnv :: Environment t -> Var -> Var
varIdentityInEnv (Env _ _ vds) (GlobalVar vn) =
  maybe (GlobalVar vn) (LocalVar vn) (elemIndex vn (map fst vds))
varIdentityInEnv env (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

pushLocalVarEnv :: [(VarName, Tp)] -> Environment t -> Environment t
pushLocalVarEnv nvds (Env cls gv vds) = Env cls gv (reverse nvds ++ vds)

-- the function returns the environment unchanged if a pattern and its type 
-- are not compatible in the sense of the following function
pushPatternEnv :: Pattern -> Tp -> Environment t -> Environment t
pushPatternEnv (VarP vn) t (Env cls gv vds) = Env cls gv  ((vn, t):vds)
pushPatternEnv (VarListP vns) (TupleT ts) (Env cls gv vds) = Env cls gv (reverse (zip vns ts) ++ vds)
pushPatternEnv _ _ env = env

-- a pattern and its type are compatible
compatiblePatternType :: Pattern -> Tp -> Bool
compatiblePatternType (VarP vn) t = True
compatiblePatternType (VarListP vns) (TupleT ts) = length vns == length ts
compatiblePatternType _ _ = False

-- compatibleType extends subclassing to all type constructors. 
-- compatibleType env t1 t2 roughly means that t1 is a subtype of t2
compatibleType :: Environment t -> Tp -> Tp -> Bool
compatibleType _ BoolT BoolT = True
compatibleType _ IntT IntT = True
compatibleType env (ClassT c1) (ClassT c2) = isSubclassOf env c1 c2
compatibleType env (FunT dom1 cod1) (FunT dom2 cod2) =
  compatibleType env dom2 dom1 && compatibleType env cod1 cod2
compatibleType env (TupleT ts1) (TupleT ts2) =
  (length ts1 == length ts2) &&
  all (uncurry (compatibleType env)) (zip ts1 ts2)
compatibleType _ _ _ = False


{-
newtype Fix f = Fixa (f (Fix f))

data ExpF a = Bin a a | T

pattern BinX a b = Fixa (Bin a b)

type ExpR = Fix ExpF

cata :: (f a -> b) -> Fix f -> b 
cata = _

trans :: (f a -> g a) -> Fix f -> Fix g
trans = _
-}


{-
-- 
data family LocTypeAnnotFam a
data instance LocTypeAnnotFam (Expr t) = LTAFExpr t
data instance LocTypeAnnotFam (Assertion t) = LTAFAssertion t
-}

getTypeOfExpr :: TypeAnnot f => Expr (f a) -> a
getTypeOfExpr = getType . annotOfExpr


tpExpr :: TypeAnnot f => Environment t -> Expr (f a) -> Expr (f Tp)
tpExpr env x = case x of
  ValE annot c -> ValE (updType annot (tpConstval env c)) c
  VarE annot v -> VarE (updType annot (tpVar env v)) (varIdentityInEnv env v)
  UnaOpE annot uop e ->
    let te = tpExpr env e
        t  = tpUnaop env (getTypeOfExpr te) uop
    in  UnaOpE (updType annot t) uop te
  BinOpE annot bop e1 e2 ->
    let te1 = tpExpr env e1
        te2 = tpExpr env e2
        t   = tpBinop env (getTypeOfExpr te1) (getTypeOfExpr te2) bop
    in  BinOpE (updType annot t) bop te1 te2
  IfThenElseE annot c e1 e2 ->
    let tc = tpExpr env c
        te1 = tpExpr env e1
        te2 = tpExpr env e2
        t1 = getTypeOfExpr te1
        t2 = getTypeOfExpr te2
    in
      if isBooleanTp (getTypeOfExpr tc)
      then if compatibleType env t1 t2
           then IfThenElseE (updType annot t2) tc te1 te2
           else if compatibleType env t2 t1
                then IfThenElseE (updType annot t1) tc te1 te2
                else IfThenElseE (updType annot ErrT) tc te1 te2
      else  IfThenElseE (updType annot ErrT) tc te1 te2
  AppE annot fe ae ->
    let tfe = tpExpr env fe
        tae = tpExpr env ae
        tf  = getTypeOfExpr tfe
        ta  = getTypeOfExpr tae
    in
      case tf of
      FunT tpar tbody ->
        if compatibleType env ta tpar
        then AppE (updType annot tbody) tfe tae
        else AppE (updType annot ErrT) tfe tae
      _ -> AppE (updType annot ErrT) tfe tae
  FunE annot pt tparam e ->
    let te = tpExpr (pushPatternEnv pt tparam env) e
        t  = getTypeOfExpr te
    in
      -- the recursive call comes before the test should
      -- because even in case of an error, a typed subexpression has to be computed
      if compatiblePatternType pt tparam
      then FunE (updType annot (FunT tparam t)) pt tparam te
      else FunE (updType annot ErrT) pt tparam te
  QuantifE annot q vn vt e ->
    let te = tpExpr (pushLocalVarEnv [(vn, vt)] env) e
    in
      if isBooleanTp (getTypeOfExpr te)
      then QuantifE (updType annot booleanT) q vn vt te
      else QuantifE (updType annot ErrT) q vn vt te
  FldAccE annot e fn ->
    let te = tpExpr env e
        t = getTypeOfExpr te
    in case t of
      ClassT cn ->
        case lookup fn (map (\(FieldDecl _ fn tp) -> (fn, tp)) (fieldsOf env cn)) of
          Nothing -> FldAccE (updType annot ErrT) te fn
          Just ft -> FldAccE (updType annot ft) te fn
      _ -> FldAccE (updType annot ErrT) te fn
  TupleE annot es ->
    let tes = map (tpExpr env) es
        ts = map getTypeOfExpr tes
    in
      if any isErrTp ts
      then TupleE (updType annot ErrT) tes
      else TupleE (updType annot (TupleT ts)) tes
  CastE annot ctp e ->
    let te = tpExpr env e
    in if castCompatible (getTypeOfExpr te) ctp
       then CastE (updType annot ctp) ctp te
       else CastE (updType annot ErrT) ctp te
  NotDeriv annot sign v e ->
    let tv = tpVar env v
        te = tpExpr env e
        t = getTypeOfExpr te
    in case tv of
      FunT tpar tbody ->
        if compatibleType env t tpar
        then
          if isBooleanTp tbody
          then NotDeriv (updType annot booleanT) sign v te
          else NotDeriv (updType annot ErrT) sign v te
        else NotDeriv (updType annot ErrT) sign v te
      _ -> NotDeriv (updType annot ErrT) sign v te

  _ -> error "typing of lists not implemented yet"


tpExprBasic :: Environment t -> Expr SRng -> Expr AnnotTypingPhase
tpExprBasic env e = fmap PosTpAnnotTP (tpExpr env (fmap (\r -> LocTypeAnnot r ()) e))

-- TODO:FAssign
tpCmd :: TypeAnnot f => Environment [ClassName] -> Cmd (f a) -> Cmd (f Tp)
tpCmd env Skip = Skip
tpCmd env (VAssign v e) =
    let te = tpExpr env e
    in
      if tpVar env v == getTypeOfExpr te
      then VAssign v te
      else error "types do not correspond in assignment"
tpCmd env (FAssign _ _ _) = error "typing of FAssign not implemented yet"


-- TODO: hack, see later
tpRuleVarDecls :: [VarDecl SRng] -> [VarDecl AnnotTypingPhase]
tpRuleVarDecls = map (\(VarDecl annot vn t) -> VarDecl (PosAnnotTP annot) vn t)

-- TODO: still take local variables into account
tpRule :: Environment t -> Rule SRng -> Rule AnnotTypingPhase
tpRule env (Rule annot rn vds precond postcond) =
  let renv = pushLocalVarEnv (map (\(VarDecl _ vn vt) -> (vn, vt)) vds) env
      tpdVds = tpRuleVarDecls vds 
  in Rule (PosAnnotTP annot) rn tpdVds (tpExprBasic renv precond) (tpExprBasic renv postcond)

tpAssertion :: Environment t -> Assertion SRng -> Assertion AnnotTypingPhase
tpAssertion env (Assertion annot e) = Assertion (PosAnnotTP annot) (tpExprBasic env e)

-- TODO: check types of global variable declarations
-- Assumption: prelude only contains class declarations
tpProgram :: Program SRng -> Program SRng -> Program AnnotTypingPhase
tpProgram prelude (Program annot lex cds gvars rls asrt) =
  let pcds = classDeclsOfProgram prelude
      initialClassDecls = (pcds ++ cds)
      elabClassDecls = elaborateClsProgram initialClassDecls
      env = initialEnvOfProgram elabClassDecls gvars
  in Program (PosAnnotTP annot) (map (fmap PosAnnotTP) lex) (map (fmap PosAnnotTP) elabClassDecls) (map (fmap PosAnnotTP) gvars) (map (tpRule env) rls) (map (tpAssertion env) asrt)


----------------------------------------------------------------------
-- Typing Timed Automata
----------------------------------------------------------------------

clockOfConstraint :: ClConstr -> Clock
clockOfConstraint (ClCn c _ _) = c

-- TODO: move into preamble file
listSubset :: Eq a => [a] -> [a] -> Bool
listSubset xs ys = all (`elem` ys) xs

-- TODO: move into preamble file
distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x : xs) =  notElem x xs && distinct xs


wellFormedAction :: [ClassName] -> Action -> Bool
wellFormedAction ta_act_clss Internal = True
wellFormedAction ta_act_clss (Act cn _) = cn `elem` ta_act_clss


-- TODO: still type-check expression e
wellFormedTransitionCond :: TypeAnnot f => [Clock] -> TransitionCond (f a) -> Bool
wellFormedTransitionCond ta_clks (TransCond ccs e) =
  listSubset (map clockOfConstraint ccs) ta_clks

-- TODO: still type-check command c
wellFormedTransitionAction :: TypeAnnot f => [ClassName] -> [Clock] -> TransitionAction (f a) -> Bool
wellFormedTransitionAction ta_act_clss ta_clks (TransAction act clks c) =
  wellFormedAction ta_act_clss act &&
  listSubset clks ta_clks

wellFormedTransition :: TypeAnnot f => [Loc] -> [ClassName] -> [Clock] -> Transition (f a) -> Bool
wellFormedTransition ta_locs ta_act_clss ta_clks (Trans l1 trcond tract l2) =
  elem l1 ta_locs && elem l2 ta_locs &&
  wellFormedTransitionCond ta_clks trcond &&
  wellFormedTransitionAction ta_act_clss ta_clks tract

typeTransitionCond :: TypeAnnot f => Environment [ClassName] -> TransitionCond (f a) -> TransitionCond (f Tp)
typeTransitionCond env (TransCond ccs e) = TransCond ccs (tpExpr env e)

typeTransitionAction :: TypeAnnot f => Environment [ClassName] -> TransitionAction (f a) -> TransitionAction (f Tp)
typeTransitionAction env (TransAction act clks c) = TransAction act clks (tpCmd env c)

typeTransition :: TypeAnnot f => Environment [ClassName] -> Transition (f a) -> Transition (f Tp)
typeTransition env (Trans l1 trcond tract l2) =
  Trans l1 (typeTransitionCond env trcond) (typeTransitionAction env tract) l2

wellFormedTA :: TypeAnnot f => Environment [ClassName] -> TA (f a) -> TA (f Tp)
wellFormedTA env (TmdAut nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) =
  if
    all (wellFormedTransition ta_locs ta_act_clss ta_clks) trans &&
    all (\c -> isSubclassOf env c (ClsNm "Event")) ta_act_clss &&
    all (\(l, ccs) -> elem l ta_locs && listSubset (map clockOfConstraint ccs) ta_clks) invs
  then
    let lbls_locs = map fst lbls
        tes = map (tpExpr env . snd) lbls
        ttrans = map (typeTransition env) trans
    in
      if all (`elem` ta_locs) lbls_locs && all (\te -> getTypeOfExpr te == BoolT) tes
      then TmdAut nm ta_locs ta_act_clss ta_clks ttrans init_locs invs (zip lbls_locs tes)
      else error "ill-formed timed automaton (labels)"
  else error "ill-formed timed automaton (transitions)"

wellFormedTASys :: TypeAnnot f => Environment [ClassName] -> TASys (f a) ext -> TASys (f Tp) ext
wellFormedTASys env (TmdAutSys tas ext) =
  if distinct (map name_of_ta tas)
  then TmdAutSys (map (wellFormedTA env) tas) ext
  else error "duplicate TA names"


