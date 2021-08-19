-- Typing of expressions
{-
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-}

{-# LANGUAGE TypeFamilies #-}

module Typing where

import Data.List ( elemIndex, nub )
import Data.Maybe
import Data.Either (isLeft, lefts, fromRight)
import Data.Either.Utils (fromLeft)
import Data.List.Utils


import Annotation
    ( LocTypeAnnot(LocTypeAnnot, typeAnnot), SRng, TypeAnnot(..), HasLoc(..) )
import Error
import Syntax




----------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------

-- Typing is done in an environment, composed of
-- the class decls, global and local variable declarations
type VarEnvironment = [(VarName, Tp ())]

-- Environment of all defined classes
type KindEnvironment = [ClassName]

data Environment t = Env { classDeclsOfEnv :: [ClassDecl t]
                         , globalsOfEnv :: VarEnvironment
                         , localsOfEnv :: VarEnvironment}
  deriving (Eq, Ord, Show, Read)

-- prg is the current program to be typed
initialEnvOfProgram :: [ClassDecl t] -> [VarDecl t] -> Environment t
initialEnvOfProgram cds gvars =
  let initialGvs = map (\(VarDecl _ vn t) -> (vn, ()<$t)) gvars
  in Env cds initialGvs []


----------------------------------------------------------------------
-- Elementary functions
----------------------------------------------------------------------


-- TODO: move into preamble file
distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x : xs) =  notElem x xs && distinct xs

eraseAnn :: Tp t -> Tp ()
eraseAnn t = () <$ t


----------------------------------------------------------------------
-- Class manipulation
----------------------------------------------------------------------

classDefAssoc :: [ClassDecl t] -> [(ClassName, ClassDef t)]
classDefAssoc = map (\(ClassDecl _ cn cdf) -> (cn, cdf))

fieldAssoc ::  [ClassDecl t] -> [(ClassName, [FieldDecl t])]
fieldAssoc = map (\(ClassDecl _ cn cdf) -> (cn, fieldsOfClassDef cdf))


-- For a class name 'cn', returns
--   - either the list of the names of the (non-strict) superclasses of 'cn' (Right: correct result)
--   - or (one of) the class names involved in a cyclic hierarchy (Left: corresponding to an error situation)
-- Here, 'cdf_assoc' is an association of class names and class defs as contained in a program.
-- 'visited' is the list of class names already visited on the way up the class hierarchy
superClassesConstr :: [(ClassName, ClassDef t)] -> [ClassName] -> ClassName -> Either ClassName [ClassName]
superClassesConstr cdf_assoc visited cn =
  case lookup cn cdf_assoc of
    -- the following should not happen if definedSuperclass is true in a module
    Nothing -> error "in superClassesConstr: cn not in cdf_assoc (internal error)"
    -- reached the top of the hierarchy
    Just (ClassDef [] _) -> Right (reverse (cn : visited))
    -- class has super-class with name scn
    Just (ClassDef [scn] _) ->
      if scn `elem` visited
      then  Left cn
      else superClassesConstr cdf_assoc (cn : visited) scn
    Just (ClassDef _ _) -> error "in superClassesConstr: superclass list should be empty or singleton (internal error)"


superClasses :: [(ClassName, ClassDef t)] -> ClassName -> [ClassName]
superClasses cdf_assoc cn =
  case superClassesConstr cdf_assoc [] cn of
    Right cns -> cns
    Left cnc  ->  error (("cyclic superclass hierarchy for class " ++ (case cnc of (ClsNm n) -> n)) ++
                          "Internal error: superClasses should not be called on cyclic hierarchy")


elaborateSupersInClassDecl :: (ClassName -> [ClassName]) -> ClassDecl t -> ClassDecl t
elaborateSupersInClassDecl supers (ClassDecl annot cn (ClassDef _ fds)) =
  ClassDecl annot cn (ClassDef (supers cn) fds)

-- in a class declaration, replace the reference to the immediate super-class
-- by the list of all super-classes (from more to less specific, excluding the current class from the list)
elaborateSupersInClassDecls :: [ClassDecl t] -> [ClassDecl t]
elaborateSupersInClassDecls cds =
  let cdf_assoc = classDefAssoc cds
  in map (elaborateSupersInClassDecl (superClasses cdf_assoc)) cds


localFields :: [(ClassName, [FieldDecl t])] -> ClassName -> [FieldDecl t]
localFields fd_assoc cn =
  fromMaybe [] (lookup cn fd_assoc)

-- in a class declaration, replace the list of local fields of the class by the list of all fields (local and inherited)
elaborateFieldsInClassDecls :: [ClassDecl t] -> [ClassDecl t]
elaborateFieldsInClassDecls cds =
  let fd_assoc = fieldAssoc cds
  in map (\(ClassDecl annot cn (ClassDef scs locfds)) ->
            ClassDecl annot cn (ClassDef scs (concatMap (localFields fd_assoc) scs))) cds

-- the class decl cdc does not reference a superclass undefined in the list of defined class names cns
definedSuperclass :: [ClassName] -> ClassDecl t -> Bool
definedSuperclass cns cdc = all (`elem` cns) (supersOfClassDef (defOfClassDecl cdc))

duplicates :: (Ord a) => [a] -> [a]
duplicates xs = nub [x | x <- xs, countElem x xs > 1]

duplicatesWrtFun :: (Ord b) => (a -> b) -> [a] -> [a]
duplicatesWrtFun f xs = [x | x <- xs, countElem (f x) (map f xs) > 1]


superClassesOf :: Environment t -> ClassName -> [ClassName]
superClassesOf env cn = case lookup cn (classDefAssoc (classDeclsOfEnv env)) of
  -- TODO: not call error (causes a crash for lsp server)
  Nothing -> error ("in superclassesOf: undefined class " ++ (case cn of (ClsNm n) -> n))
  Just (ClassDef supcls _) -> supcls

strictSuperClassesOf :: Environment t -> ClassName -> [ClassName]
strictSuperClassesOf env cn = tail (superClassesOf env cn)

isStrictSubclassOf :: Environment t -> ClassName -> ClassName -> Bool
isStrictSubclassOf env subcl supercl = supercl `elem` strictSuperClassesOf env subcl

isSubclassOf :: Environment t -> ClassName -> ClassName -> Bool
isSubclassOf env subcl supercl = supercl `elem` superClassesOf env subcl

-- TODO: determining the sub- / superclasses does not require an environment after elaboration of superclasses.
-- Maybe some of the above functions become redundant.

superClassesOfClassDecl :: ClassDecl t -> [ClassName]
superClassesOfClassDecl = supersOfClassDef . defOfClassDecl

strictSuperClassesOfClassDecl :: ClassDecl t -> [ClassName]
strictSuperClassesOfClassDecl = tail . superClassesOfClassDecl


-- Get all the fields (direct and inherited) associated with a class name in an environment
-- function should only be called with a class name known in the environment 
fieldsOf :: Environment t -> ClassName -> [FieldDecl t]
fieldsOf env cn = case lookup cn (classDefAssoc (classDeclsOfEnv env)) of
  Nothing -> error ("internal error in fieldsOf: undefined class " ++ (case cn of (ClsNm n) -> n))
  Just (ClassDef _ fds) -> fds

longestCommonPrefix :: Eq a=> [a] -> [a] -> [a]
longestCommonPrefix (x:xs) (y:ys) = if x == y then x:longestCommonPrefix xs ys else []
longestCommonPrefix _ _ = []

-- least common superclass of two classes (given by their name)
-- that must at least have Object as common superclass
leastCommonSuperClass :: Environment t -> ClassName -> ClassName -> ClassName
leastCommonSuperClass env cn1 cn2 =
  last (longestCommonPrefix (reverse (superClassesOf env cn1)) (reverse (superClassesOf env cn2)))

leastCommonSuperType :: Environment te -> Tp t -> Tp t -> Tp ()
leastCommonSuperType env (ClassT _ cn1) (ClassT _ cn2) = ClassT () (leastCommonSuperClass env cn1 cn2)
leastCommonSuperType _ _ _ = error "internal errror: leastCommonSuperType should only be called on class types"

-- find all class definitions for class with name cn in environment
-- (assuming possibly duplicate definitions which would be an error)
-- TODO: this function is redundant with the functions above
-- Remove after error processing of classes and expressions is clearer
lookupClassDefInEnv :: Environment t -> ClassName -> [ClassDef t]
lookupClassDefInEnv env cn =
  map defOfClassDecl (filter (\cd -> nameOfClassDecl cd == cn) (classDeclsOfEnv env))


----------------------------------------------------------------------
-- Checking types wrt. a kind environment
----------------------------------------------------------------------

kndTypeCombine :: b ->  [Either [a] b] -> Either [a] b
kndTypeCombine t es =
  let ls = lefts es
  in case ls of
    [] -> Right t
    _ -> Left (concat ls)

-- generic version
kndType :: KindEnvironment -> Tp t -> Either [ClassName] (Tp t)
kndType kenv c@(ClassT ann cn) = if cn `elem` kenv then Right c else Left [cn]
kndType kenv t@(FunT _ a b)  = kndTypeCombine t (map (kndType kenv) [a, b])
kndType kenv t@(TupleT _ ts) = kndTypeCombine t (map (kndType kenv) ts)
kndType kenv t = Right t

-- instance
kndTypeI :: KindEnvironment -> Tp SRng -> Either [ClassName] (Tp (LocTypeAnnot (Tp ())))
kndTypeI kenv c@(ClassT ann cn) = if cn `elem` kenv then Right (ClassT (setType ann KindT) cn) else Left [cn]
kndTypeI kenv t@(FunT ann a b)  =
  let aI = kndTypeI kenv a
      bI = kndTypeI kenv b
  in if isLeft aI || isLeft bI
     then Left (concat (lefts [aI, bI]))
     else Right (FunT (setType ann KindT) (fromRight KindT aI) (fromRight KindT bI))
--kndTypeI kenv t@(TupleT _ ts) = kndTypeCombine t (map (kndTypeI kenv) ts)
--kndTypeI kenv t = Right t


-- simple
-- ClassT (Annot Rng0 (Errt Undefinedtypeinclasst))
-- FunT _ (ClassT (Annot rng0 KindT) (Classn c1)) (ClassT (Annot Rng1 (Errt Undefinedtypeinclasst)))
kndTypeS :: KindEnvironment -> Tp (LocTypeAnnot (Tp ())) -> Tp (LocTypeAnnot (Tp ()))
kndTypeS kenv (ClassT ann cn) =
  if cn `elem` kenv
  then ClassT (updType ann KindT) cn
  else ClassT (updType ann (ErrT UndefinedTypeInClassT)) cn
kndTypeS kenv (FunT ann a b) =
  let aI = kndTypeS kenv a
      bI = kndTypeS kenv b
      resT = propagateError (map (typeAnnot . annotOfTp) [aI, bI]) KindT
  in FunT (updType ann resT) aI bI
kndTypeS kenv (TupleT ann ts) =
  let subTs = map (kndTypeS kenv) ts
      resT = propagateError (map (typeAnnot . annotOfTp) subTs) KindT
  in TupleT (updType ann resT) subTs
kndTypeS kenv (ErrT c) = ErrT c
kndTypeS kenv OkT = OkT
kndTypeS kenv KindT = KindT



----------------------------------------------------------------------
-- Linking classes from the prelude to internal predicates
----------------------------------------------------------------------


isBooleanTp :: Tp t -> Bool
isBooleanTp (ClassT _ (ClsNm "Boolean")) = True
isBooleanTp _ = False

isNumberTp :: Environment te -> Tp t -> Bool
isNumberTp env (ClassT _ t) = isSubclassOf env t (ClsNm "Number")
isNumberTp _ _ = False

isIntegerTp :: Tp t -> Bool
isIntegerTp (ClassT _ (ClsNm "Integer")) = True
isIntegerTp _ = False

isFloatTp :: Tp t -> Bool
isFloatTp (ClassT _ (ClsNm "Float")) = True
isFloatTp _ = False

isTimeTp :: Tp t -> Bool
isTimeTp (ClassT _ (ClsNm "Time")) = True
isTimeTp _ = False

isScalarTp :: Tp t -> Bool
isScalarTp FunT {} = False
isScalarTp (TupleT _ ts) = all isScalarTp ts
isScalarTp _ = True

isErrTp :: Tp t -> Bool
isErrTp ErrT {} = True
isErrTp _ = False

isClassTp :: Tp t -> Bool
isClassTp ClassT {} = True
isClassTp _ = False

----------------------------------------------------------------------
-- Typing functions
----------------------------------------------------------------------

propagateError :: [Tp t] -> Tp t -> Tp t
propagateError ts t =
  if any isErrTp ts
  then ErrT Inherited
  else t


-- TODO: when removinv RecordV, the environment becomes superfluous
tpConstval ::  Val -> Tp ()
tpConstval x = case x of
  BoolV _ -> booleanT
  IntV _ -> integerT
  FloatV _ -> floatT 
  StringV _ -> stringT
  ErrV -> ErrT Inherited

  {-
  -- for record values to be well-typed, the fields have to correspond exactly (incl. order of fields) to the class fields.
  -- TODO: maybe relax some of these conditions.
  RecordV cn fnvals ->
    -- list of: field name, type of value
    let tfnvals = map (\(fn, v) -> (fn, (tpConstval env v))) fnvals
    in case lookupClassDefInEnv env cn of
       [] -> error ("class name " ++ (case cn of (ClsNm n) -> n) ++ " not defined")
       [cd] ->
         if map (\(FieldDecl _ fn t) -> (fn, t)) (fieldsOfClassDef cd) == tfnvals
         then ClassT () cn
         else error ("record fields do not correspond to fields of class " ++ (case cn of (ClsNm n) -> n))
       _ -> error "internal error: duplicate class definition"
  -}
  
tpUarith :: Environment te -> [SRng] -> Tp t -> UArithOp -> Tp t
tpUarith env locs t ua =
  if isNumberTp env t
  then t
  else ErrT (IllTypedSubExpr locs  [eraseAnn t] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))])

-- applicable to both unary boolean as temporal modal operators
tpUbool :: [SRng] -> Tp t -> Tp t
tpUbool locs t =
  if isBooleanTp t
  then t
  else ErrT (IllTypedSubExpr locs [eraseAnn t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))])

tpUnaop :: Environment te -> [SRng] -> Tp t -> UnaOp -> Tp t
tpUnaop env locs t uop =
  propagateError [t]
  (case uop of
    UArith ua  -> tpUarith env locs t ua
    UBool _   -> tpUbool locs t
    UTemporal _ -> tpUbool locs t
  )

-- TODO: The error message is inappropriate. Change when reworking the type checking algorithm
tpTime :: [SRng] -> Tp () -> Tp () -> BArithOp -> Tp ()
tpTime _ _ _  BAadd = ClassT () (ClsNm "Time")
tpTime _ _ _  BAsub = ClassT () (ClsNm "Time")
tpTime locs t1 t2 _ = ErrT (IllTypedSubExpr [locs!!0,locs!!1] [t1] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))])

tpBarith :: Environment te -> [SRng] -> Tp () -> Tp () -> BArithOp -> Tp ()
tpBarith env locs t1 t2 ba =
  if isNumberTp env t1
  then if isNumberTp env t2
       then 
         if isTimeTp t1 || isTimeTp t2
         then tpTime locs t1 t2 ba
         else leastCommonSuperType env t1 t2
       else ErrT (IllTypedSubExpr [locs!!0,locs!!2] [t2] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))])
  else ErrT (IllTypedSubExpr [locs!!0,locs!!1] [t1] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))])

-- TODO: more liberal condition for comparison?
tpBcompar :: Environment te -> [SRng] -> Tp () -> Tp () -> BComparOp -> Tp ()
tpBcompar env locs t1 t2 bc =
  if isScalarTp t1 && isScalarTp t2
  then if compatibleType env t1 t2 || compatibleType env t2 t1
       then booleanT
       else ErrT (IncompatibleTp locs [t1, t2])
  else ErrT (NonScalarExpr locs [t1, t2])

tpBbool :: Environment te -> [SRng] -> Tp () -> Tp () -> BBoolOp -> Tp ()
tpBbool env locs t1 t2 bc =
  if isBooleanTp t1
  then if isBooleanTp t2
       then booleanT
       else ErrT (IllTypedSubExpr [locs!!0,locs!!2] [t2] [ExpectedExactTp booleanT])
  else ErrT (IllTypedSubExpr [locs!!0,locs!!1] [t1] [ExpectedExactTp booleanT])

tpBinop :: Environment te -> [SRng] -> Tp () -> Tp () -> BinOp -> Tp ()
tpBinop env locs t1 t2 bop =
  propagateError [t1, t2]
  (case bop of
    BArith ba  -> tpBarith env locs t1 t2 ba
    BCompar bc -> tpBcompar env locs t1 t2 bc
    BBool bb   -> tpBbool env locs t1 t2 bb
  )

-- the first type can be cast to the second type
-- TODO: still to be defined
castCompatible :: Tp t -> Tp t -> Bool
castCompatible te ctp = True


-- typing of a variable that is initially (after parsing) only known by its name
tpVar :: Environment te -> SRng -> Var t -> Tp ()
tpVar env loc (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in case lookup vn (globalsOfEnv env) of
      Nothing ->Data.Maybe.fromMaybe (ErrT (UndeclaredVariable loc vn)) (lookup vn (localsOfEnv env))
      Just t -> t
tpVar env _ (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

varIdentityInEnv :: Environment te -> Var t -> Var t
varIdentityInEnv (Env _ _ vds) (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in maybe (GlobalVar qvn) (LocalVar qvn) (elemIndex vn (map fst vds))
varIdentityInEnv env (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

pushLocalVarEnv :: VarEnvironment -> Environment t -> Environment t
pushLocalVarEnv nvds (Env cls gv vds) = Env cls gv (reverse nvds ++ vds)

-- the function returns the environment unchanged if a pattern and its type
-- are not compatible in the sense of the following function
pushPatternEnv :: Pattern t -> Tp t -> Environment te -> Environment te
pushPatternEnv (VarP vn) t (Env cls gv vds) =
  Env cls gv  ((nameOfQVarName vn, eraseAnn t):vds)
pushPatternEnv (VarListP vns) (TupleT _ ts) (Env cls gv vds) =
  Env cls gv (reverse (zip (map nameOfQVarName vns) (map eraseAnn ts)) ++ vds)
pushPatternEnv _ _ env = env

-- a pattern and its type are compatible
compatiblePatternType :: Pattern t -> Tp t -> Bool
compatiblePatternType (VarP vn) t = True
compatiblePatternType (VarListP vns) (TupleT _ ts) = length vns == length ts
compatiblePatternType _ _ = False

-- compatibleType extends subclassing to all type constructors.
-- compatibleType env t1 t2 roughly means that t1 is a subtype of t2
compatibleType :: Environment te -> Tp t -> Tp t -> Bool
compatibleType env (ClassT _ c1) (ClassT _ c2) = isSubclassOf env c1 c2
compatibleType env (FunT _ dom1 cod1) (FunT _ dom2 cod2) =
  compatibleType env dom2 dom1 && compatibleType env cod1 cod2
compatibleType env (TupleT _ ts1) (TupleT _ ts2) =
  length ts1 == length ts2 &&
  all (uncurry (compatibleType env)) (zip ts1 ts2)
compatibleType _ _ _ = False


getTypeOfExpr :: TypeAnnot f => Expr (f a) -> a
getTypeOfExpr = getType . annotOfExpr

setType :: SRng -> a -> LocTypeAnnot a
setType = LocTypeAnnot

tpExpr :: Environment te -> Expr (LocTypeAnnot (Tp())) -> Expr (LocTypeAnnot (Tp()))
tpExpr env x = case x of
  ValE annot c -> ValE (updType annot (tpConstval c)) c
  VarE annot v -> VarE (updType annot (tpVar env (getLoc annot) v)) (varIdentityInEnv env v)

  UnaOpE annot uop e ->
    let te = tpExpr env e
        t  = tpUnaop env [getLoc annot, getLoc e] (getTypeOfExpr te) uop
    in  UnaOpE (updType annot t) uop te

  BinOpE annot bop e1 e2 ->
    let te1 = tpExpr env e1
        te2 = tpExpr env e2
        t   = tpBinop env [getLoc annot, getLoc e1, getLoc e2] (getTypeOfExpr te1) (getTypeOfExpr te2) bop
    in  BinOpE (updType annot t) bop te1 te2


  -- TODO: consider a more liberal typing returning the least common supertype of the two branches
  IfThenElseE annot ec e1 e2 ->
    let tec = tpExpr env ec
        te1 = tpExpr env e1
        te2 = tpExpr env e2
        tc = getTypeOfExpr tec
        t1 = getTypeOfExpr te1
        t2 = getTypeOfExpr te2
        restp = propagateError [tc, t1, t2]
               (if isBooleanTp tc
                then if compatibleType env t1 t2
                     then t2
                     else if compatibleType env t2 t1
                          then t1
                          else ErrT (IncompatibleTp [getLoc annot, getLoc e1, getLoc e2] [t1, t2])
                else ErrT (IllTypedSubExpr [getLoc annot, getLoc ec] [tc] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]))
    in IfThenElseE (updType annot restp) tec te1 te2


  AppE annot fe ae ->
    let tfe = tpExpr env fe
        tae = tpExpr env ae
        tf  = getTypeOfExpr tfe
        ta  = getTypeOfExpr tae
        restp = propagateError [tf, ta]
                (case tf of
                  FunT _ tpar tbody ->
                    if compatibleType env ta tpar
                    then tbody
                    else ErrT (IllTypedSubExpr [getLoc annot, getLoc ae] [ta] [ExpectedSubTpOf tpar])
                  _ -> ErrT (NonFunctionTp [getLoc annot, getLoc fe] tf))
    in AppE (updType annot restp) tfe tae

  FunE annot pt tparam e ->
    let te = tpExpr (pushPatternEnv pt tparam env) e
        t  = getTypeOfExpr te
        restp = propagateError [t]
                 (if compatiblePatternType pt tparam
                  then FunT () (eraseAnn tparam) t
                  else ErrT (IncompatiblePattern (getLoc annot)))
    in FunE (updType annot restp) pt tparam te

  QuantifE annot q vn vt e ->
    let te = tpExpr (pushLocalVarEnv [(nameOfQVarName vn, eraseAnn vt)] env) e
        t  = getTypeOfExpr te
        restp = propagateError [t]
                (if isBooleanTp t
                 then booleanT
                 else ErrT (IllTypedSubExpr [getLoc annot, getLoc e] [t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))] ))
    in QuantifE (updType annot restp) q vn vt te

  FldAccE annot e fn ->
    let te = tpExpr env e
        t = getTypeOfExpr te
        restp = propagateError [t]
                  (case t of
                    ClassT _ cn ->
                      case lookup fn (map (\(FieldDecl _ fn tp) -> (fn, tp)) (fieldsOf env cn)) of
                        Nothing -> ErrT (UnknownFieldName (getLoc e) fn cn)
                        Just ft -> eraseAnn ft
                    _ -> ErrT (AccessToNonObjectType (getLoc e))
                  )
    in FldAccE (updType annot restp) te fn

  TupleE annot es ->
    let tes = map (tpExpr env) es
        ts = map getTypeOfExpr tes
    in TupleE (updType annot (propagateError ts (TupleT () ts))) tes

  CastE annot ctp e ->
    let te = tpExpr env e
        t = getTypeOfExpr te
        ctpEr = eraseAnn ctp
        restp = propagateError [t]
                (if castCompatible t ctpEr
                 then ctpEr
                 else ErrT (CastIncompatible [getLoc annot, getLoc e] t ctpEr))
    in CastE (updType annot restp) ctp te

  NotDeriv annot sign e ->
    let te = tpExpr env e
        t = getTypeOfExpr te
        restp = propagateError [t]
                (if isBooleanTp t
                 then booleanT
                 else ErrT (IllTypedSubExpr [getLoc annot, getLoc e] [t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))] ))
    in NotDeriv (updType annot restp) sign te
  _ -> error "typing of lists not implemented yet"


tpVarDecl :: [ClassName] -> VarDecl (LocTypeAnnot (Tp ()))  -> VarDecl (LocTypeAnnot (Tp ()))
tpVarDecl kenv (VarDecl ann vn tp) =
  let annotTp = kndTypeS kenv tp
  in  VarDecl (updType ann (eraseAnn annotTp)) vn annotTp

tpRule :: Environment t -> Rule (LocTypeAnnot (Tp ()))  -> Rule (LocTypeAnnot (Tp ()))
tpRule env (Rule ann rn instr vds precond postcond) =
  let renv = pushLocalVarEnv (map (\(VarDecl _ vn vt) -> (vn, eraseAnn vt)) vds) env
      teprecond  = tpExpr renv precond
      tepostcond = tpExpr renv postcond
      tprecond = getTypeOfExpr teprecond
      tpostcond = getTypeOfExpr tepostcond
      kenv = map nameOfClassDecl (classDeclsOfEnv env)
      tpdVds = map (tpVarDecl kenv) vds
      restp = propagateError ([tprecond, tpostcond] ++ [getType (annotOfVarDecl tv) | tv <- tpdVds, isSpecificError (getType (annotOfVarDecl tv)) ])
              (if isBooleanTp tprecond
               then if isBooleanTp tpostcond
                    then tpostcond
                    else ErrT (IllTypedSubExpr [getLoc ann, getLoc postcond] [tpostcond] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))])
                else ErrT (IllTypedSubExpr [getLoc ann, getLoc precond] [tprecond] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))])
              )
  in Rule (updType ann (eraseAnn restp)) rn instr tpdVds teprecond tepostcond

tpAssertion :: Environment t -> Assertion (LocTypeAnnot (Tp ()))  -> Assertion (LocTypeAnnot (Tp ()))
tpAssertion env (Assertion ann nm md e) =
  let te = tpExpr env e
      t  = getTypeOfExpr te
      restp = propagateError [t]
              (if isBooleanTp t
               then t
               else ErrT (IllTypedSubExpr [getLoc ann, getLoc e] [t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]))
  in Assertion (updType ann (eraseAnn restp)) nm md te


----------------------------------------------------------------------
-- Class declaration errors


checkClassesForWfError :: HasLoc t => [ClassDecl t] -> Program t -> Either ClassDeclsError (Program t)
checkClassesForWfError cds prg =
  let class_names = map nameOfClassDecl cds
  in
    case filter (not . definedSuperclass class_names) cds of
      [] -> case duplicates class_names of
              [] -> Right prg
              ds -> Left (DuplicateClassNamesCDE
                    [(getLoc cd, nameOfClassDecl cd) | cd <- cds, nameOfClassDecl cd `elem` ds])
      undefs -> Left (UndefinedSuperclassCDE (map (\cd -> (getLoc cd, nameOfClassDecl cd)) undefs))

checkClassesForCyclicError :: HasLoc t => [ClassDecl t] -> Program t -> Either ClassDeclsError (Program t)
checkClassesForCyclicError cds prg =
  let cdf_assoc = classDefAssoc cds
      cyclicClassNames = lefts (map (superClassesConstr cdf_assoc [] . nameOfClassDecl) cds)
  in case cyclicClassNames of
     []   -> Right (prg {classDeclsOfProgram = elaborateFieldsInClassDecls (elaborateSupersInClassDecls cds)})
     cycs -> Left (CyclicClassHierarchyCDE
              [ (getLoc cd, nameOfClassDecl cd)| cd <- cds, nameOfClassDecl cd `elem` cycs])

checkClassDeclsError :: HasLoc t => Program t -> Program t -> Either ClassDeclsError (Program t)
checkClassDeclsError prelude prg@(Program  annot lex cds gvars rls asrt) =
  let pcds = classDeclsOfProgram prelude
      initialClassDecls = (pcds ++ cds)
  in do
    checkClassesForWfError initialClassDecls prg
    checkClassesForCyclicError initialClassDecls prg


----------------------------------------------------------------------
-- Field declaration errors

checkDuplicateFieldNamesFDE ::HasLoc t =>  Program t -> Either FieldDeclsError (Program t)
checkDuplicateFieldNamesFDE prg =
  let classDeclsWithDup = [cd | cd <- classDeclsOfProgram prg, not (null (duplicates (map nameOfFieldDecl ((fieldsOfClassDef . defOfClassDecl)  cd)))) ]
  in case classDeclsWithDup of
    [] -> Right prg
    cds -> Left (DuplicateFieldNamesFDE
                  (map (\cd -> (getLoc cd, nameOfClassDecl cd,
                        map (\fd -> (getLoc fd, nameOfFieldDecl fd)) (duplicatesWrtFun nameOfFieldDecl (fieldsOfClassDef (defOfClassDecl cd)))))
                   cds))

checkUndefinedTypeFDE :: HasLoc t => Program t -> Either FieldDeclsError (Program t)
checkUndefinedTypeFDE prg =
  let kenv = map nameOfClassDecl (classDeclsOfProgram prg)
      classDeclsWithUndefTp = [cd | cd <- classDeclsOfProgram prg, not (null (lefts (map (kndType kenv . tpOfFieldDecl) (fieldsOfClassDef (defOfClassDecl cd)))))]
  in case classDeclsWithUndefTp of
    [] -> Right prg
    cds -> Left (UndefinedTypeFDE (concatMap (\cd -> [(getLoc fd, nameOfFieldDecl fd) | fd <- fieldsOfClassDef (defOfClassDecl cd), isLeft (kndType kenv (tpOfFieldDecl fd))  ]) cds))

checkFieldDeclsError ::HasLoc t =>  Program t -> Either FieldDeclsError (Program t)
checkFieldDeclsError prg =
  do
    checkDuplicateFieldNamesFDE prg
    checkUndefinedTypeFDE prg


----------------------------------------------------------------------
-- Global variable declaration errors

checkDuplicateVarNamesVDE :: HasLoc t => Program t -> Either VarDeclsError (Program t)
checkDuplicateVarNamesVDE prg =
  case duplicatesWrtFun nameOfVarDecl (globalsOfProgram  prg) of
    [] -> Right prg
    vds -> Left (DuplicateVarNamesVDE (map (\vd -> (getLoc vd, nameOfVarDecl vd)) vds))

checkUndefinedTypeVDE :: HasLoc t => Program t -> Either VarDeclsError (Program t)
checkUndefinedTypeVDE prg =
  let kenv = map nameOfClassDecl (classDeclsOfProgram prg)
      varDeclsWithUndefTp = [vd | vd <- globalsOfProgram prg, isLeft (kndType kenv (tpOfVarDecl vd))]
  in case varDeclsWithUndefTp of
    [] -> Right prg
    vds -> Left (UndefinedTypeVDE (map (\vd -> (getLoc vd, nameOfVarDecl vd)) vds))

checkVarDeclsError :: HasLoc t => Program t -> Either VarDeclsError (Program t)
checkVarDeclsError prg =
  do
    checkDuplicateVarNamesVDE prg
    checkUndefinedTypeVDE prg


----------------------------------------------------------------------
-- Errors in Rules and Assertions


isSpecificError :: Tp t -> Bool
isSpecificError e =
  case e of
    ErrT Inherited -> False
    ErrT Unspecified -> False
    ErrT _ -> True
    _ -> False

extractErrorCause :: Tp t -> ErrorCause
extractErrorCause (ErrT c) = c
extractErrorCause _ = Unspecified

displayableErrors :: (TypeAnnot f, HasLoc (f (Tp()))) => SRng -> f (Tp()) -> [(SRng, ErrorCause)]
displayableErrors rng annot = [(rng, extractErrorCause (getType annot)) | isSpecificError (getType annot)]

extractRuleErrors::  (TypeAnnot f, HasLoc (f (Tp()))) => Rule (f (Tp())) -> [(SRng, ErrorCause)]
extractRuleErrors rl =
  let rng = getLoc (annotOfRule rl) 
  in concatMap (displayableErrors rng . annotOfVarDecl) (varDeclsOfRule rl) ++
     concatMap (displayableErrors rng . annotOfExpr) (allSubExprs (precondOfRule rl)) ++
     concatMap (displayableErrors rng . annotOfExpr) (allSubExprs (postcondOfRule rl)) ++
     displayableErrors rng (annotOfRule rl)

checkRuleError :: Program (LocTypeAnnot (Tp ())) -> Either RuleError (Program (LocTypeAnnot (Tp ())))
checkRuleError prg =
  let env = initialEnvOfProgram (classDeclsOfProgram prg) (globalsOfProgram prg)
      tpdRules = map (tpRule env) (rulesOfProgram prg)
      errs = concatMap extractRuleErrors tpdRules
  in
    case errs of
      [] -> Right (prg {rulesOfProgram = tpdRules})
      _ ->  Left (RuleErrorRE  errs)


extractAssertionErrors :: (TypeAnnot f, HasLoc (f (Tp()))) => Assertion (f (Tp())) -> [(SRng, ErrorCause)]
extractAssertionErrors ass =
  let rng = getLoc (annotOfAssertion ass) 
  in concatMap (displayableErrors rng . annotOfExpr) (allSubExprs (exprOfAssertion ass)) ++
     displayableErrors rng (annotOfAssertion ass)

--checkAssertionError :: (TypeAnnot f, HasLoc (f (Tp()))) => Program (f (Tp())) -> Either AssertionError (Program (f (Tp())))
checkAssertionError :: Program (LocTypeAnnot (Tp ())) -> Either AssertionError (Program (LocTypeAnnot (Tp ())))
checkAssertionError prg =
  let env = initialEnvOfProgram (classDeclsOfProgram prg) (globalsOfProgram prg)
      tpdAss = map (tpAssertion env) (assertionsOfProgram prg)
  in
    if any (isErrTp . getType . annotOfAssertion) tpdAss || any (isErrTp . getTypeOfExpr . exprOfAssertion) tpdAss
    then Left (AssertionErrAE (concatMap extractAssertionErrors tpdAss))
    else Right (prg {assertionsOfProgram = tpdAss})

----------------------------------------------------------------------
-- Summing up: all errors

liftLeft :: (a -> la) -> Either a b -> Either la b
liftLeft f (Left a) = Left (f a)
liftLeft f (Right r) = Right r

-- the lifted version of the checkError function below
-- checkErrorGen :: (TypeAnnot f, HasLoc (f (Tp()))) => Program (f (Tp())) -> Program (f (Tp())) -> Either Error (Program (f (Tp())))
checkErrorLift :: Program (LocTypeAnnot (Tp ())) -> Program (LocTypeAnnot (Tp ())) -> Either Error (Program (LocTypeAnnot (Tp ())))
checkErrorLift prelude prg =
  do
    prgClsDecls <- liftLeft ClassDeclsErr (checkClassDeclsError prelude prg)
    liftLeft FieldDeclsErr (checkFieldDeclsError prgClsDecls)
    liftLeft VarDeclsErr (checkVarDeclsError prgClsDecls)
    prgCheckRule <- liftLeft RuleErr (checkRuleError prgClsDecls)
    liftLeft AssertionErr (checkAssertionError prgCheckRule)

liftLoc :: SRng -> LocTypeAnnot (Tp ())
liftLoc rng  = LocTypeAnnot rng OkT

checkError :: Program SRng -> Program SRng -> Either Error (Program (LocTypeAnnot (Tp())))
checkError prelude prg = checkErrorLift (fmap liftLoc prelude) (fmap liftLoc prg)


----------------------------------------------------------------------
-- Typing Timed Automata
----------------------------------------------------------------------
-- TOOD: the following section about Timed Automata is legacy code.
{-

tpCmd :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> Cmd (f a) -> Cmd (f Tp)
tpCmd env (Skip annot) = Skip (updType annot OkT)
tpCmd env (VAssign annot v e) =
    let te = tpExpr env e
        t = getTypeOfExpr te
        tv = tpVar env (getLoc annot) v       -- TODO: the location is the one of the whole assingment and not of the variable alone
        restp = propagateError [t, tv]
                (if compatibleType env t tv
                 then OkT
                 else ErrT (IllTypedSubExpr [getLoc annot, getLoc e] [t] [ExpectedSubTpOf tv ])
                )
    in VAssign (updType annot restp) v te
tpCmd env FAssign {} = error "typing of FAssign not implemented yet"

clockOfConstraint :: ClConstr -> Clock
clockOfConstraint (ClConstr c _ _) = c

-- TODO: move into preamble file
listSubset :: Eq a => [a] -> [a] -> Bool
listSubset xs ys = all (`elem` ys) xs


wellFormedAction :: [ClassName] -> Action -> Bool
wellFormedAction ta_act_clss Internal = True
wellFormedAction ta_act_clss (Act cn _) = cn `elem` ta_act_clss


-- TODO: still type-check expression e
wellFormedTransitionGuard :: TypeAnnot f => [Clock] -> TransitionGuard (f a) -> Bool
wellFormedTransitionGuard ta_clks (TransitionGuard ccs e) =
  listSubset (map clockOfConstraint ccs) ta_clks

-- TODO: still type-check command c
wellFormedTransitionAction :: TypeAnnot f => [ClassName] -> [Clock] -> TransitionAction (f a) -> Bool
wellFormedTransitionAction ta_act_clss ta_clks (TransitionAction act clks c) =
  wellFormedAction ta_act_clss act &&
  listSubset clks ta_clks

wellFormedTransition :: TypeAnnot f => [Loc] -> [ClassName] -> [Clock] -> Transition (f a) -> Bool
wellFormedTransition ta_locs ta_act_clss ta_clks (Transition l1 trcond tract l2) =
  elem l1 ta_locs && elem l2 ta_locs &&
  wellFormedTransitionGuard ta_clks trcond &&
  wellFormedTransitionAction ta_act_clss ta_clks tract

typeTransitionGuard :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> TransitionGuard (f a) -> TransitionGuard (f Tp)
typeTransitionGuard env (TransitionGuard ccs e) = TransitionGuard ccs (tpExpr env e)

typeTransitionAction :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> TransitionAction (f a) -> TransitionAction (f Tp)
typeTransitionAction env (TransitionAction act clks c) = TransitionAction act clks (tpCmd env c)

typeTransition :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> Transition (f a) -> Transition (f Tp)
typeTransition env (Transition l1 trcond tract l2) =
  Transition l1 (typeTransitionGuard env trcond) (typeTransitionAction env tract) l2

wellFormedTA :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> TA (f a) -> TA (f Tp)
wellFormedTA env (TA nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) =
  if
    all (wellFormedTransition ta_locs ta_act_clss ta_clks) trans &&
    all (\c -> isSubclassOf env c (ClsNm "Event")) ta_act_clss &&
    all (\(l, ccs) -> elem l ta_locs && listSubset (map clockOfConstraint ccs) ta_clks) invs
  then
    let lbls_locs = map fst lbls
        tes = map (tpExpr env . snd) lbls
        ttrans = map (typeTransition env) trans
    in
      if all (`elem` ta_locs) lbls_locs && all (\te -> getTypeOfExpr te == booleanT) tes
      then TA nm ta_locs ta_act_clss ta_clks ttrans init_locs invs (zip lbls_locs tes)
      else error "ill-formed timed automaton (labels)"
  else error "ill-formed timed automaton (transitions)"

wellFormedTASys :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> TASys (f a) ext -> TASys (f Tp) ext
wellFormedTASys env (TASys tas ext) =
  if distinct (map name_of_ta tas)
  then TASys (map (wellFormedTA env) tas) ext
  else error "duplicate TA names"

-}
