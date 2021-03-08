-- Typing of expressions

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
{-
newtype GlobalVarDecls = GVD VarEnvironment
  deriving (Eq, Ord, Show, Read)
newtype LocalVarDecls = LVD VarEnvironment
  deriving (Eq, Ord, Show, Read)
  -}
data Environment t = Env { classDeclsOfEnv :: [ClassDecl t]
                         , globalsOfEnv :: VarEnvironment
                         , localsOfEnv :: VarEnvironment }
  deriving (Eq, Ord, Show, Read)

{-
classDeclsOfEnv :: Environment t -> [ClassDecl t]
classDeclsOfEnv (Env m _ _) = m

globalsOfEnv :: Environment t -> [(VarName,Tp)]
globalsOfEnv (Env _ (GVD gs) _) = gs

localsOfEnv :: Environment t -> [(VarName,Tp)]
localsOfEnv (Env _ _ (LVD ls)) = ls
-}
initialEnvOfProgram :: [ClassDecl (Maybe ct)]  -> Program (Maybe ct) et -> Environment (Maybe ct)
initialEnvOfProgram cds prg =
  let initialGvs = map (\(VarDecl vn t) -> (vn, t)) (globalsOfProgram prg)
  in Env cds initialGvs []

-- TODO: recheck the typing

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


-- in a class declaration, replace the reference to the immediate super-class by the list of all super-classes
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
elaborateClsProgram :: Program (Maybe ClassName) et -> Program [ClassName] et
elaborateClsProgram prg =
  let cds = classDeclsOfProgram prg
  in  if wellformedClassDecls cds
      then
        let ecdcs = elaborateFieldsInClassDecls (elaborateSupersInClassDecls cds)
        in
          if all wellFormedFieldDecls ecdcs
          then prg { classDeclsOfProgram = ecdcs }
          else error "Problem in field declarations: duplicate field declarations"
      else error "Problem in class declarations"


strictSuperclassesOf :: [ClassDecl [ClassName]] -> ClassName -> [ClassName]
strictSuperclassesOf cds cn = case lookup cn (classDefAssoc cds) of
  Nothing -> error ("in strictSuperclassesOf: undefined class " ++ (case cn of (ClsNm n) -> n))
  Just (ClassDef supcls _) -> supcls

isStrictSuperclassesOf :: [ClassDecl [ClassName]] -> ClassName -> ClassName -> Bool
isStrictSuperclassesOf cds subcl supercl = subcl `elem` (strictSuperclassesOf cds subcl)

isSubclassOf :: [ClassDecl [ClassName]] -> ClassName -> ClassName -> Bool
isSubclassOf cds subcl supercl = subcl == supercl || isStrictSuperclassesOf cds subcl supercl


----------------------------------------------------------------------
-- Typing functions
----------------------------------------------------------------------

lookupClassDefInEnv :: Environment t -> ClassName -> [ClassDef t]
lookupClassDefInEnv env cn =
  map def_of_class_decl (filter (\cd -> name_of_class_decl cd == cn) (classDeclsOfEnv env))

tpConstval :: Environment t -> Val -> Tp
tpConstval env x = case x of
  BoolV _ -> BoolT
  IntV _ -> IntT
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


tpUarith :: Tp -> UArithOp -> Tp
tpUarith t ua = if t == IntT then IntT else ErrT

tpUbool :: Tp -> UBoolOp -> Tp
tpUbool t ub = if t == BoolT then BoolT else ErrT

tpUnaop :: Tp -> UnaOp -> Tp
tpUnaop t uop = case uop of
  UArith ua  -> tpUarith t ua
  UBool ub   -> tpUbool t ub


tpBarith :: Tp -> Tp -> BArithOp -> Tp
tpBarith t1 t2 ba = if t1 == t2 && t1 == IntT then IntT else ErrT

tpBcompar :: Tp -> Tp -> BComparOp -> Tp
tpBcompar t1 t2 bc = if t1 == t2 then BoolT else ErrT

tpBbool :: Tp -> Tp -> BBoolOp -> Tp
tpBbool t1 t2 bc = if t1 == t2 && t1 == BoolT then BoolT else ErrT

tpBinop :: Tp -> Tp -> BinOp -> Tp
tpBinop t1 t2 bop = case bop of
  BArith ba  -> tpBarith t1 t2 ba
  BCompar bc -> tpBcompar t1 t2 bc
  BBool bb   -> tpBbool t1 t2 bb


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

pushPatternEnv :: Pattern -> Tp -> Environment t -> Environment t
pushPatternEnv (VarP vn) t (Env cls gv vds) = Env cls gv  ((vn, t):vds)
pushPatternEnv (VarListP vns) (TupleT ts) (Env cls gv vds) = Env cls gv (reverse (zip vns ts) ++ vds)

compatiblePatternType :: Pattern -> Tp -> Bool
compatiblePatternType (VarP vn) t = True
compatiblePatternType (VarListP vns) (TupleT ts) = length vns == length ts
compatiblePatternType _ _ = False

-- TODO: FldAccE, ListE
tpExpr :: Environment t -> Expr () -> Expr Tp
tpExpr env x = case x of
  ValE () c -> ValE (tpConstval env c) c
  VarE () v -> VarE (tpVar env v) (varIdentityInEnv env v)
  UnaOpE () uop e ->
    let te = tpExpr env e
        t  = tpUnaop (tpOfExpr te) uop
    in  UnaOpE t uop te
  BinOpE () bop e1 e2 ->
    let te1 = tpExpr env e1
        te2 = tpExpr env e2
        t   = tpBinop (tpOfExpr te1) (tpOfExpr te2) bop
    in  BinOpE t bop te1 te2
  IfThenElseE () c e1 e2 ->
    let tc = tpExpr env c
        te1 = tpExpr env e1
        te2 = tpExpr env e2
    in
      if tpOfExpr tc == BoolT && tpOfExpr te1 == tpOfExpr te2
      then IfThenElseE (tpOfExpr te1) tc te1 te2
      else  IfThenElseE ErrT tc te1 te2
  AppE () fe ae ->
    let tfe = tpExpr env fe
        tae = tpExpr env ae
        tf  = tpOfExpr tfe
        ta  = tpOfExpr tae
    in case tf of
      FunT tpar tbody ->
        if tpar == ta
        then AppE tbody tfe tae
        else AppE ErrT tfe tae
      _ -> AppE ErrT tfe tae
  FunE () pt tparam e ->
    let te = tpExpr (pushPatternEnv pt tparam env) e
        t  = tpOfExpr te
    in
      -- TODO: the test should come before the recursive call
      -- because pushPatternEnv may lead to a Haskell match failure.
      if compatiblePatternType pt tparam
      then FunE (FunT tparam t) pt tparam te
      else FunE ErrT pt tparam te
  -- ClosE: no explicit typing because not externally visible
  QuantifE () q vn vt e ->
    let te = tpExpr (pushLocalVarEnv [(vn, vt)] env) e
    in
      if tpOfExpr te == BoolT
      then QuantifE BoolT q vn vt te
      else QuantifE ErrT q vn vt te
  CastE () ctp e ->
    let te = tpExpr env e
    in if castCompatible (tpOfExpr te) ctp
       then CastE ctp ctp te
       else CastE ErrT ctp te



tpCmd :: Environment t -> Cmd () -> Cmd Tp
tpCmd env Skip = Skip
tpCmd env (VAssign v e) =
    let te = tpExpr env e
    in
      if tpVar env v == tpOfExpr te
      then VAssign v te
      else error "types do not correspond in assignment"


-- TODO: still take local variables into account
tpRule :: Environment t -> Rule () -> Rule Tp
tpRule env (Rule rn vds precond postcond) =
  let renv = pushLocalVarEnv (map (\(VarDecl vn vt) -> (vn, vt)) vds) env
  in Rule rn vds (tpExpr renv precond) (tpExpr renv postcond)
tpAssertion :: Environment t -> Assertion () -> Assertion Tp
tpAssertion env (Assertion e) = Assertion (tpExpr env e)

-- TODO: check types of global variable declarations
-- Assumption: prelude only contains class declarations
tpProgram :: Program (Maybe ClassName) () -> Program (Maybe ClassName) () -> Program [ClassName] Tp
tpProgram prelude prg =
  let pcls = classDeclsOfProgram prelude
      initialClassDecls = (customCs ++ pcls ++ classDeclsOfProgram prg)
      env = initialEnvOfProgram initialClassDecls prg
      (Program lex allCls gvars rls asrt) = elaborateClsProgram (prg {classDeclsOfProgram = initialClassDecls})
  in Program lex allCls gvars (map (tpRule env) rls) (map (tpAssertion env) asrt)


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
    all (\act_cls -> isSubclassOf (classDeclsOfEnv env) act_cls (ClsNm "Event")) ta_act_clss &&
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


