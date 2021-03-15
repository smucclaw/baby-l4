-- Typing of expressions

{-# LANGUAGE PatternSynonyms #-}

module Typing where

import Data.List
import Data.Maybe
import Syntax

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
initialEnvOfProgram :: [ClassDecl t] -> [VarDecl] -> Environment t
initialEnvOfProgram cds gvars =
  let initialGvs = map (\(VarDecl vn t) -> (vn, t)) gvars
  in Env cds initialGvs []


----------------------------------------------------------------------
-- Class manipulation
----------------------------------------------------------------------

classDefAssoc :: [ClassDecl t] -> [(ClassName, ClassDef t)]
classDefAssoc = map (\(ClassDecl cn cdf) -> (cn, cdf))

fieldAssoc ::  [ClassDecl t] -> [(ClassName, [FieldDecl])]
fieldAssoc = map (\(ClassDecl cn cdf) -> (cn, fields_of_class_def cdf))


-- For a class name 'cn', returns the list of the names of the superclasses of 'cn'
-- Here, 'cdf_assoc' is an association of class names and class defs as contained in a program.
-- 'visited' is the list of class names already visited on the way up the class hierarchy
superClasses :: [(ClassName, ClassDef (Maybe ClassName))] -> [ClassName] -> ClassName -> [ClassName]
superClasses cdf_assoc visited cn =
  case lookup cn cdf_assoc of
    -- the following should not happen if definedSuperclass is true in a module
    Nothing -> error "in superClasses: cn not in cdf_assoc (internal error)"
    -- reached the top of the hierarchy
    Just (ClassDef Nothing _) -> reverse (cn : visited)
    -- class has super-class with name scn
    Just (ClassDef (Just scn) _) ->
      if scn `elem` visited
      then error ("cyclic superclass hierarchy for class " ++ (case cn of (ClsNm n) -> n))
      else superClasses cdf_assoc (cn : visited) scn

-- For each of a list of class declarations, returns its list of superclass names
superClassesDecls :: [ClassDecl (Maybe ClassName)] -> [[ClassName]]
superClassesDecls cds =
  let cdf_assoc = classDefAssoc cds
  in map (superClasses cdf_assoc [] . fst) cdf_assoc


-- in a class declaration, replace the reference to the immediate super-class 
-- by the list of all super-classes (from more to less specific, excluding the current class from the list)
elaborateSupersInClassDecls :: [ClassDecl (Maybe ClassName)] -> [ClassDecl [ClassName]]
elaborateSupersInClassDecls cds =
  let cdf_assoc = classDefAssoc cds
  in map (\(ClassDecl cn (ClassDef mcn fds)) ->
    ClassDecl cn (ClassDef (tail (superClasses cdf_assoc [] cn)) fds)) cds


localFields :: [(ClassName, [FieldDecl])] -> ClassName -> [FieldDecl]
localFields fd_assoc cn =
  fromMaybe [] (lookup cn fd_assoc)

-- in a class declaration, replace the list of local fields of the class by the list of all fields (local and inherited)
elaborateFieldsInClassDecls :: [ClassDecl [ClassName]] -> [ClassDecl [ClassName]]
elaborateFieldsInClassDecls cds =
  let fd_assoc = fieldAssoc cds
  in map (\(ClassDecl cn (ClassDef scs locfds)) ->
            ClassDecl cn (ClassDef scs (locfds ++ concatMap (localFields fd_assoc) scs))) cds


-- the class decl does not reference an undefined superclass
definedSuperclass :: [ClassName] -> ClassDecl (Maybe ClassName) -> Bool
definedSuperclass cns cdc =
  case cdc of
    (ClassDecl cn (ClassDef Nothing _)) -> True
    (ClassDecl cn (ClassDef (Just scn) _)) ->
      elem scn cns || error ("undefined superclass for class " ++ (case cn of (ClsNm n) -> n))


hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

wellformedClassDecls :: [ClassDecl (Maybe ClassName)] -> Bool
wellformedClassDecls cds =
  let class_names = map name_of_class_decl cds
  in all (definedSuperclass class_names) cds && not (hasDuplicates class_names)

-- TODO: still check that field decls only reference declared classes
wellFormedFieldDecls :: ClassDecl t -> Bool
wellFormedFieldDecls (ClassDecl cn cdf) = not (hasDuplicates (fields_of_class_def cdf))

-- TODO: a bit of a hack. Error detection and treatment to be improved
-- - no ref to undefined superclass
-- - no cyclic graph hierarchy (implemented in superClasses above)
-- - no duplicate field declarations (local and inherited)
elaborateClsProgram :: [ClassDecl (Maybe ClassName)] -> [ClassDecl [ClassName]]
elaborateClsProgram cds =
  if wellformedClassDecls cds
  then
    let ecdcs = elaborateFieldsInClassDecls (elaborateSupersInClassDecls cds)
    in
      if all wellFormedFieldDecls ecdcs
      then ecdcs
      else error "Problem in field declarations: duplicate field declarations"
  else error "Problem in class declarations"


strictSuperclassesOf :: Environment [ClassName] -> ClassName -> [ClassName]
strictSuperclassesOf env cn = case lookup cn (classDefAssoc (classDeclsOfEnv env)) of
  Nothing -> error ("in strictSuperclassesOf: undefined class " ++ (case cn of (ClsNm n) -> n))
  Just (ClassDef supcls _) -> supcls

superclassesOf :: Environment [ClassName] -> ClassName -> [ClassName]
superclassesOf env cn = cn : strictSuperclassesOf env cn

isStrictSubclassOf :: Environment [ClassName] -> ClassName -> ClassName -> Bool
isStrictSubclassOf env subcl supercl = supercl `elem` strictSuperclassesOf env subcl

isSubclassOf :: Environment [ClassName] -> ClassName -> ClassName -> Bool
isSubclassOf env subcl supercl = supercl `elem` superclassesOf env subcl

longestCommonPrefix :: Eq a=> [a] -> [a] -> [a]
longestCommonPrefix (x:xs) (y:ys) = if x == y then x:longestCommonPrefix xs ys else []
longestCommonPrefix _ _ = []

-- least common superclass of two classes (given by their name) 
-- that must at least have Object as common superclass
leastCommonSuperClass :: Environment [ClassName] -> ClassName -> ClassName -> ClassName
leastCommonSuperClass env cn1 cn2 =
  last (longestCommonPrefix (reverse (superclassesOf env cn1)) (reverse (superclassesOf env cn2)))

leastCommonSuperType :: Environment [ClassName] -> Tp -> Tp -> Tp
leastCommonSuperType env (ClassT cn1) (ClassT cn2) = ClassT (leastCommonSuperClass env cn1 cn2)
leastCommonSuperType _ _ _ = error "internal errror: leastCommonSuperType should only be called on class types"

-- find all class definitions for class with name cn in environment
-- (assuming possibly duplicate definitions which would be an error)
-- TODO: this function is redundant with the functions above
-- Remove after error processing of classes and expressions is clearer
lookupClassDefInEnv :: Environment t -> ClassName -> [ClassDef t]
lookupClassDefInEnv env cn =
  map def_of_class_decl (filter (\cd -> name_of_class_decl cd == cn) (classDeclsOfEnv env))


----------------------------------------------------------------------
-- Linking classes from the prelude to internal predicates
----------------------------------------------------------------------

booleanT = (ClassT (ClsNm "Boolean"))
integerT = (ClassT (ClsNm "Integer"))

isBooleanTp :: Tp -> Bool
isBooleanTp BoolT = True     -- TODO: BoolT currently still tolerated, but to be phased out
isBooleanTp (ClassT (ClsNm "Boolean")) = True
isBooleanTp _ = False

-- Note: IntT not taken as number type
isNumberTp :: Environment [ClassName] -> Tp -> Bool
isNumberTp env (ClassT t) = isSubclassOf env t (ClsNm "Number")
isNumberTp _ _ = False

isScalarType :: Tp -> Bool
isScalarType BoolT = True
isScalarType IntT = True
isScalarType (ClassT _) = True
isScalarType (FunT _ _) = False
isScalarType (TupleT ts) = all isScalarType ts
isScalarType ErrT = True

----------------------------------------------------------------------
-- Typing functions
----------------------------------------------------------------------


tpConstval :: Environment t -> Val -> Tp
tpConstval env x = case x of
  BoolV _ -> booleanT
  IntV _ -> integerT
  -- for record values to be well-typed, the fields have to correspond exactly (incl. order of fields) to the class fields.
  -- TODO: maybe relax some of these conditions.
  RecordV cn fnvals ->
    -- list of: field name, type of value
    let tfnvals = map (\(fn, v) -> (fn, (tpConstval env v))) fnvals
    in case lookupClassDefInEnv env cn of
       [] -> error ("class name " ++ (case cn of (ClsNm n) -> n) ++ " not defined")
       [cd] ->
         if map (\(FieldDecl fn t) -> (fn, t)) (fields_of_class_def cd) == tfnvals
         then ClassT cn
         else error ("record fields do not correspond to fields of class " ++ (case cn of (ClsNm n) -> n))
       _ -> error "internal error: duplicate class definition"


tpUarith :: Environment [ClassName] -> Tp -> UArithOp -> Tp
tpUarith env t ua = if isNumberTp env t then t else ErrT

tpUbool :: Environment [ClassName] -> Tp -> UBoolOp -> Tp
tpUbool env t ub = if isBooleanTp t then t else ErrT

tpUnaop :: Environment [ClassName] -> Tp -> UnaOp -> Tp
tpUnaop env t uop = case uop of
  UArith ua  -> tpUarith env t ua
  UBool ub   -> tpUbool env t ub


tpBarith :: Environment [ClassName] -> Tp -> Tp -> BArithOp -> Tp
tpBarith env t1 t2 ba =
  if isNumberTp env t1 && isNumberTp env t2
    then leastCommonSuperType env t1 t2
    else ErrT

-- TODO: more liberal condition for comparison?
tpBcompar :: Environment [ClassName] -> Tp -> Tp -> BComparOp -> Tp
tpBcompar env t1 t2 bc =
  if isScalarType t1 && isScalarType t2
  then
    if (compatibleType env t1 t2 || compatibleType env t2 t1)
    then booleanT
    else ErrT
  else ErrT

tpBbool :: Environment [ClassName] -> Tp -> Tp -> BBoolOp -> Tp
tpBbool env t1 t2 bc =
  if isBooleanTp t1 && isBooleanTp t2 then booleanT else ErrT

tpBinop :: Environment [ClassName] -> Tp -> Tp -> BinOp -> Tp
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
compatibleType :: Environment [ClassName] -> Tp -> Tp -> Bool
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

-- TODO: FldAccE, ListE
tpExpr :: Environment [ClassName] -> Expr () -> Expr Tp
tpExpr env x = case x of
  ValE rng () c -> ValE rng (tpConstval env c) c
  VarE rng () v -> VarE rng (tpVar env v) (varIdentityInEnv env v)
  UnaOpE rng () uop e ->
    let te = tpExpr env e
        t  = tpUnaop env (tpOfExpr te) uop
    in  UnaOpE rng t uop te
  BinOpE rng () bop e1 e2 ->
    let te1 = tpExpr env e1
        te2 = tpExpr env e2
        t   = tpBinop env (tpOfExpr te1) (tpOfExpr te2) bop
    in  BinOpE rng t bop te1 te2
  IfThenElseE rng () c e1 e2 ->
    let tc = tpExpr env c
        te1 = tpExpr env e1
        te2 = tpExpr env e2
        t1 = tpOfExpr te1
        t2 = tpOfExpr te2
    in
      if isBooleanTp (tpOfExpr tc)
      then if compatibleType env t1 t2
           then IfThenElseE rng t2 tc te1 te2
           else if compatibleType env t2 t1 
                then IfThenElseE rng t1 tc te1 te2
                else IfThenElseE rng ErrT tc te1 te2
      else  IfThenElseE rng ErrT tc te1 te2
  AppE rng () fe ae ->
    let tfe = tpExpr env fe
        tae = tpExpr env ae
        tf  = tpOfExpr tfe
        ta  = tpOfExpr tae
    in case tf of
      FunT tpar tbody ->
        if compatibleType env ta tpar
        then AppE rng tbody tfe tae
        else AppE rng ErrT tfe tae
      _ -> AppE rng ErrT tfe tae
  FunE rng () pt tparam e ->
    let te = tpExpr (pushPatternEnv pt tparam env) e
        t  = tpOfExpr te
    in
      -- the recursive call comes before the test should
      -- because even in case of an error, a typed subexpression has to be computed
      if compatiblePatternType pt tparam
      then FunE rng (FunT tparam t) pt tparam te
      else FunE rng ErrT pt tparam te
  
  -- ClosE: no explicit typing because not externally visible
  QuantifE rng () q vn vt e ->
    let te = tpExpr (pushLocalVarEnv [(vn, vt)] env) e
    in
      if isBooleanTp (tpOfExpr te)
      then QuantifE rng booleanT q vn vt te
      else QuantifE rng ErrT q vn vt te
  CastE rng () ctp e ->
    let te = tpExpr env e
    in if castCompatible (tpOfExpr te) ctp
       then CastE rng ctp ctp te
       else CastE rng ErrT ctp te



tpCmd :: Environment [ClassName] -> Cmd () -> Cmd Tp
tpCmd env Skip = Skip
tpCmd env (VAssign v e) =
    let te = tpExpr env e
    in
      if tpVar env v == tpOfExpr te
      then VAssign v te
      else error "types do not correspond in assignment"


-- TODO: still take local variables into account
tpRule :: Environment [ClassName] -> Rule () -> Rule Tp
tpRule env (Rule rn vds precond postcond) =
  let renv = pushLocalVarEnv (map (\(VarDecl vn vt) -> (vn, vt)) vds) env
  in Rule rn vds (tpExpr renv precond) (tpExpr renv postcond)
tpAssertion :: Environment [ClassName] -> Assertion () -> Assertion Tp
tpAssertion env (Assertion e) = Assertion (tpExpr env e)

-- TODO: check types of global variable declarations
-- Assumption: prelude only contains class declarations
tpProgram :: Program (Maybe ClassName) () -> Program (Maybe ClassName) () -> Program [ClassName] Tp
tpProgram prelude (Program lex cds gvars rls asrt) =
  let pcds = classDeclsOfProgram prelude
      initialClassDecls = (customCs ++ pcds ++ cds)
      elabClassDecls = elaborateClsProgram initialClassDecls
      env = initialEnvOfProgram elabClassDecls gvars
  in Program lex elabClassDecls gvars (map (tpRule env) rls) (map (tpAssertion env) asrt)


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
wellFormedTransitionCond :: [Clock] -> TransitionCond () -> Bool
wellFormedTransitionCond ta_clks (TransCond ccs e) =
  listSubset (map clockOfConstraint ccs) ta_clks

-- TODO: still type-check command c
wellFormedTransitionAction :: [ClassName] -> [Clock] -> TransitionAction () -> Bool
wellFormedTransitionAction ta_act_clss ta_clks (TransAction act clks c) =
  wellFormedAction ta_act_clss act &&
  listSubset clks ta_clks

wellFormedTransition :: [Loc] -> [ClassName] -> [Clock] -> Transition () -> Bool
wellFormedTransition ta_locs ta_act_clss ta_clks (Trans l1 trcond tract l2) =
  elem l1 ta_locs && elem l2 ta_locs &&
  wellFormedTransitionCond ta_clks trcond &&
  wellFormedTransitionAction ta_act_clss ta_clks tract

typeTransitionCond :: Environment [ClassName] -> TransitionCond () -> TransitionCond Tp
typeTransitionCond env (TransCond ccs e) = TransCond ccs (tpExpr env e)

typeTransitionAction :: Environment [ClassName] -> TransitionAction () -> TransitionAction Tp
typeTransitionAction env (TransAction act clks c) = TransAction act clks (tpCmd env c)

typeTransition :: Environment [ClassName] -> Transition () -> Transition Tp
typeTransition env (Trans l1 trcond tract l2) =
  Trans l1 (typeTransitionCond env trcond) (typeTransitionAction env tract) l2

wellFormedTA :: Environment [ClassName] -> TA () -> TA Tp
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
      if all (`elem` ta_locs) lbls_locs && all (\te -> tpOfExpr te == BoolT) tes
      then TmdAut nm ta_locs ta_act_clss ta_clks ttrans init_locs invs (zip lbls_locs tes)
      else error "ill-formed timed automaton (labels)"
  else error "ill-formed timed automaton (transitions)"

wellFormedTASys :: Environment [ClassName] -> TASys () ext -> TASys Tp ext
wellFormedTASys env (TmdAutSys tas ext) =
  if distinct (map name_of_ta tas)
  then TmdAutSys (map (wellFormedTA env) tas) ext
  else error "duplicate TA names"


