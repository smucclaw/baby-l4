-- Typing of expressions
{-
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Typing where

import Data.List ( elemIndex, nub )
import Data.Maybe ( fromMaybe )
import Data.Either (lefts)
import Data.Either.Combinators (mapLeft)
import Data.List.Utils ( countElem )


import Annotation
    ( LocTypeAnnot(LocTypeAnnot, typeAnnot), SRng, TypeAnnot(..), HasLoc(..), HasAnnot (getAnnot) )
import Error
import Syntax
import Control.Applicative ((<|>), Alternative (empty))


----------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------

-- Typing is done in an environment, composed of
-- the class decls, global and local variable declarations
type VarTyping = (VarName, Tp())
type VarEnvironment = [VarTyping]

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

elaborateFieldsInClassDecl :: [(ClassName, [FieldDecl t])] -> ClassDecl t -> ClassDecl t
elaborateFieldsInClassDecl fdAssoc (ClassDecl annot cn (ClassDef scs _)) =
  ClassDecl annot cn (ClassDef scs (concatMap (localFields fdAssoc) scs))

-- in a class declaration, replace the list of local fields of the class by the list of all fields (local and inherited)
elaborateFieldsInClassDecls :: [ClassDecl t] -> [ClassDecl t]
elaborateFieldsInClassDecls cds = map (elaborateFieldsInClassDecl (fieldAssoc cds)) cds

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
strictSuperClassesOf env cn = drop 1 (superClassesOf env cn)

isStrictSubclassOf :: Environment t -> ClassName -> ClassName -> Bool
isStrictSubclassOf env subcl supercl = supercl `elem` strictSuperClassesOf env subcl

isSubclassOf :: Environment t -> ClassName -> ClassName -> Bool
isSubclassOf env subcl supercl = supercl `elem` superClassesOf env subcl

-- TODO: determining the sub- / superclasses does not require an environment after elaboration of superclasses.
-- Maybe some of the above functions become redundant.

superClassesOfClassDecl :: ClassDecl t -> [ClassName]
superClassesOfClassDecl = supersOfClassDef . defOfClassDecl

strictSuperClassesOfClassDecl :: ClassDecl t -> [ClassName]
strictSuperClassesOfClassDecl = drop 1 . superClassesOfClassDecl


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
  -- TODO: If possible, avoid partial functions like `last`
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

kndType :: KindEnvironment -> Tp Untyped -> TCEither (Tp Typed)
kndType kenv (ClassT ann cn) =
  if cn `elem` kenv
  then pure (ClassT (setType ann KindT) cn)
  else tError (UndefinedClassInType ann cn)
kndType kenv (FunT ann a b) = do
  (aI, bI) <- (,) <$> kndType kenv a <*> kndType kenv b
  return $ FunT (setType ann KindT) aI bI
kndType kenv (TupleT ann ts) = do
  subTs <- traverse (kndType kenv) ts
  return $ TupleT (setType ann KindT) subTs
kndType _kenv ErrT = pure ErrT
kndType _kenv OkT = pure OkT
kndType _kenv KindT = pure KindT


----------------------------------------------------------------------
-- Linking classes from the prelude to internal predicates
----------------------------------------------------------------------


-- checkSimpleTp :: String -> Tp t -> TCEither ()
-- checkSimpleTp _ _ = undefined

-- checkBooleanTp :: Tp t -> TCEither ()
-- checkBooleanTp = checkSimpleTp "Boolean"

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

isClassTp :: Tp t -> Bool
isClassTp ClassT {} = True
isClassTp _ = False

----------------------------------------------------------------------
-- Typing functions
----------------------------------------------------------------------

-- type TCEither t = Either [ErrorCause] t

newtype TCEither t = TCEither {getEither :: Either [ErrorCause] t}
  deriving stock Show
  deriving newtype Functor

pattern TRight :: t -> TCEither t
pattern TRight a = TCEither (Right a)
pattern TLeft :: [ErrorCause] -> TCEither t
pattern TLeft a = TCEither (Left a)

{-# COMPLETE TRight, TLeft #-}

instance Applicative TCEither where
  pure x = TRight x
  (TLeft ecs)  <*> (TLeft ecs') = TLeft $ ecs <> ecs'
  (TLeft ecs)  <*> (TRight _)   = TLeft ecs
  (TRight _)   <*> (TLeft ecs)  = TLeft ecs
  (TRight fab) <*> (TRight a)   = TRight $ fab a

instance Alternative TCEither where
  empty = TCEither (Left [])
  -- | NB: This instance will throw away the error from the first option. This might be bad!
  (TLeft _) <|> te' = te'
  te@(TRight _) <|> _ = te

-- | WARNING: This instance behaves differently from the applicative instance!
instance Monad TCEither where
  (TLeft ecs) >>= _ = TLeft ecs
  (TRight a) >>= f = f a

tError :: ErrorCause -> TCEither t
tError e = TCEither $ Left [e]

-- TODO: when removinv RecordV, the environment becomes superfluous
tpConstval ::  Val -> Tp ()
tpConstval x = case x of
  BoolV _ -> booleanT
  IntV _ -> integerT
  FloatV _ -> floatT
  StringV _ -> stringT
  ErrV -> ErrT

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

type Triple a = (a,a,a)
type Pair a = (a,a)

pair2list :: Pair a -> [a]
pair2list x = [fst x, snd x]

triple2list :: Triple a -> [a]
triple2list (x,y,z) = [x,y,z]

tpUarith :: Environment te -> Pair SRng -> Tp t -> UArithOp -> TCEither (Tp t)
tpUarith env locs t _ua =
  if isNumberTp env t
  then pure t
  else tError (IllTypedSubExpr (pair2list locs)  [eraseAnn t] [ExpectedSubTpOf numberT])

-- applicable to both unary boolean as temporal modal operators
tpUbool :: Pair SRng -> Tp t -> TCEither (Tp t)
tpUbool locs t =
  t <$ expectBool locs t

-- TODO: Don't send along lists of ranges. Be more precise
tpUnaop :: Environment te -> Pair SRng -> Tp t -> UnaOp -> TCEither (Tp t)
tpUnaop env locs t uop = case uop of
    UArith ua  -> tpUarith env locs t ua
    UBool _   -> tpUbool locs t
    UTemporal _ -> tpUbool locs t

-- TODO: The error message is inappropriate. Change when reworking the type checking algorithm
tpTime :: Triple SRng -> Tp () -> Tp () -> BArithOp -> TCEither (Tp ())
tpTime _ _ _  BAadd = pure timeT
tpTime _ _ _  BAsub = pure timeT
tpTime (loc0, loc1, _loc2) t1 _t2 _ = tError (IllTypedSubExpr [loc0,loc1] [t1] [ExpectedSubTpOf numberT])

tpBarith :: Environment te -> Triple SRng -> Tp () -> Tp () -> BArithOp -> TCEither (Tp ())
tpBarith env locs@(l0,l1,l2) t1 t2 ba =
  if isNumberTp env t1
  then if isNumberTp env t2
       then
         if isTimeTp t1 || isTimeTp t2
         then tpTime locs t1 t2 ba
         else pure (leastCommonSuperType env t1 t2)
       else tError (IllTypedSubExpr [l0, l2] [t2] [ExpectedSubTpOf numberT])
  else tError (IllTypedSubExpr [l0, l1] [t1] [ExpectedSubTpOf numberT])

-- TODO: more liberal condition for comparison?
tpBcompar :: Environment te -> Triple SRng -> Tp () -> Tp () -> BComparOp -> TCEither (Tp ())
tpBcompar env locs t1 t2 _bc =
  if isScalarTp t1 && isScalarTp t2
  then if compatibleType env t1 t2 || compatibleType env t2 t1
       then pure booleanT
       else tError (IncompatibleTp (triple2list locs) [t1, t2])
  -- TODO: This location information isn't great!
  else tError (NonScalarExpr (triple2list locs) [t1, t2])

tpBbool :: Environment te -> Triple SRng -> Tp () -> Tp () -> BBoolOp -> TCEither (Tp ())
tpBbool _env (locCtx, locT1, locT2) t1 t2 _bc = do
  -- TODO: Should be checked in parallel
  expectBool (locCtx,locT1) t1
  expectBool (locCtx,locT2) t2
  return booleanT


tpBinop :: Environment te -> Triple SRng -> Tp () -> Tp () -> BinOp -> TCEither (Tp ())
tpBinop env locs t1 t2 bop = case bop of
    BArith ba  -> tpBarith env locs t1 t2 ba
    BCompar bc -> tpBcompar env locs t1 t2 bc
    BBool bb   -> tpBbool env locs t1 t2 bb



-- the first type can be cast to the second type
-- TODO: still to be defined
castCompatible :: Tp t -> Tp t -> Bool
castCompatible te ctp = True


-- typing of a variable that is initially (after parsing) only known by its name
tpVar :: Environment te -> SRng -> Var t -> TCEither (Tp ())
tpVar env loc (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in case lookup vn (globalsOfEnv env) <|> lookup vn (localsOfEnv env) of
        Nothing -> tError (UndeclaredVariable loc vn)
        Just t -> pure t
tpVar _env _ (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

varIdentityInEnv :: Environment te -> Var t -> Var t
varIdentityInEnv (Env _ _ vds) (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in maybe (GlobalVar qvn) (LocalVar qvn) (elemIndex vn (map fst vds))
varIdentityInEnv _env (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

pushLocalVarEnv :: VarEnvironment -> Environment t -> Environment t
pushLocalVarEnv nvds (Env cls gv vds) = Env cls gv (reverse nvds ++ vds)

varDeclToVarTyping :: VarDecl t -> VarTyping
varDeclToVarTyping v = (nameOfVarDecl v, eraseAnn (tpOfVarDecl v))

pushLocalVarDecl :: VarDecl t1 -> Environment t2 -> Environment t2
pushLocalVarDecl v = pushLocalVarEnv [varDeclToVarTyping v]

pushLocalVarDecls :: [VarDecl t1] -> Environment t2 -> Environment t2
pushLocalVarDecls vds = pushLocalVarEnv (map varDeclToVarTyping vds)


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

guardMsg :: ErrorCause -> Bool -> TCEither ()
guardMsg _ True = pure ()
guardMsg e False = tError e

notBoolMsg :: SRng -> LocTypeAnnot (Tp ()) -> ErrorCause
notBoolMsg loc t = IllTypedSubExpr [loc, getLoc t] [typeAnnot t] [ExpectedExactTp booleanT]

-- checkExpectSubtypeOf :: (HasAnnot f, TypeCheck f) => Tp () -> Environment te -> SRng -> f Untyped -> TCEither (f Typed, Tp ()) -- Hmm?
-- checkExpectExactType :: (HasAnnot f, TypeCheck f) => Tp () -> Environment te -> SRng -> f Untyped -> TCEither (f Typed)
-- checkExpectBooleanType :: (HasAnnot f, TypeCheck f) => Environment te -> SRng -> f Untyped -> TCEither (f Typed)
-- expectBooleanType :: (HasAnnot f) => SRng -> f Typed -> TCEither (Tp ())
checkBooleanType :: SRng -> LocTypeAnnot (Tp ()) -> TCEither (Tp ())
checkBooleanType loc tl = t <$ guardMsg (notBoolMsg loc tl) (isBooleanTp t)
  where t = typeAnnot tl

checkBoolTp :: HasAnnot f => SRng -> f Typed -> TCEither (Tp ())
checkBoolTp loc te = checkBooleanType loc (getAnnot te)

-- TODO: Make generic exact type helper
{-# DEPRECATED expectBool "Use checkBoolTp instead if possible" #-}
expectBool :: Pair SRng -> Tp t -> TCEither ()
expectBool locs t = 
    guardMsg  notBooMsg' (isBooleanTp t)
  where
    notBooMsg' = IllTypedSubExpr (pair2list locs) [eraseAnn t] [ExpectedExactTp booleanT]

-- TODO: Make a class for type checking
-- TODO: Make a combined function for calling recursive type checker and verifying that it has expected type
-- TODO: Clean up the tuple thingies (using similar structure as with the checkBoolTp)
-- TODO: Make TCEither an instance of an appropriate error monad class
-- TODO: Make a reader monad with Environment and the location context

-- | Verifies that the first type is a subtype of the second. If so, it returns the supertype
checkCompatLeft :: Environment te -> SRng -> LocTypeAnnot (Tp ()) -> LocTypeAnnot (Tp ()) -> TCEither (Tp ())
checkCompatLeft env loc (LocTypeAnnot loc1 t1) (LocTypeAnnot loc2 t2) = t2 <$ guardMsg incompatMsg (compatibleType env t1 t2)
  where
    incompatMsg :: ErrorCause
    incompatMsg = IncompatibleTp [loc, loc1, loc2] [t1, t2]


-- | Returns the supertype if either of the two given types is a subtype of the other
checkCompat :: Environment te -> SRng -> LocTypeAnnot (Tp ()) -> LocTypeAnnot (Tp ()) -> TCEither (Tp ())
checkCompat env ann t1 t2 = checkCompatLeft env ann t1 t2 <|> checkCompatLeft env ann t2 t1


-- TODO: check well-formedness of types in function abstraction and in quantification
tpExpr :: Environment te -> Expr Untyped -> TCEither (Expr Typed)
tpExpr env expr = case expr of
  ValE annot c -> return $ ValE (setType annot (tpConstval c)) c
  VarE annot v -> do
    tRes <- tpVar env (getLoc annot) v
    return $ VarE (setType annot tRes) (varIdentityInEnv env $ addDummyTypes v)

  UnaOpE annot uop e -> do
    te <- tpExpr env e
    tRes <- tpUnaop env (annot, getLoc e) (getTypeOfExpr te) uop
    return $ UnaOpE (setType annot tRes) uop te

  BinOpE annot bop e1 e2 -> do
    (te1, te2) <- (,) <$> tpExpr env e1 <*> tpExpr env e2
    tRes <- tpBinop env (getLoc annot, getLoc e1, getLoc e2) (getTypeOfExpr te1) (getTypeOfExpr te2) bop
    return $ BinOpE (setType annot tRes) bop te1 te2

  -- TODO: consider a more liberal typing returning the least common supertype of the two branches
  IfThenElseE annot ec e1 e2 -> do
    -- Combine error messages from all three sub-expressions
    -- Note that we could do better, since the condition check is independent from the compat check.
    -- ApplicativeDo could solve this, but might be a bit too magic
    (tc, t1, t2) <- (,,) <$> tpExpr env ec <*> tpExpr env e1 <*> tpExpr env e2
    _ <- checkBoolTp annot tc
    tRes <- checkCompat env annot (getAnnot t1) (getAnnot t2)
    return $ IfThenElseE (setType annot tRes) tc t1 t2


  AppE annot fe ae -> do
    -- TODO: Clean this up more
    let
      isFunction tf = case tf of
        FunT _ tpar tbody -> pure (tpar, tbody)
        _ -> tError (NonFunctionTp [getLoc annot, getLoc fe] tf)
      isCompatible ta tpar tbody =
        if compatibleType env ta tpar
        then pure tbody
        else tError (IllTypedSubExpr [getLoc annot, getLoc ae] [ta] [ExpectedSubTpOf tpar])

    (tfe, tae) <- (,) <$> tpExpr env fe <*> tpExpr env ae
    let tf  = getTypeOfExpr tfe
        ta  = getTypeOfExpr tae
    (tpar, tbody) <- isFunction tf
    tRes <- isCompatible ta tpar tbody
    return $ AppE (setType annot tRes) tfe tae

  FunE annot v e -> do
    tv <- tpVarDecl env v
    te <- tpExpr (pushLocalVarDecl v env) e
    let t  = getTypeOfExpr te
        tres = FunT () (eraseAnn (tpOfVarDecl v)) t
    return $ FunE (setType annot tres) tv te

  QuantifE annot q v e -> do
    tv <- tpVarDecl env v
    te <- tpExpr (pushLocalVarDecl tv env) e
    tres <- booleanT <$ checkBoolTp annot te
    return $ QuantifE (setType annot tres) q tv te

  FldAccE annot e fn -> do
    te <- tpExpr env e
    let t  = getTypeOfExpr te
    -- TODO: Clean up more
    cn <- case t of
        ClassT _ cn -> pure cn
        _ -> tError (AccessToNonObjectType (getLoc e))
    tres <- case lookup fn $ map (\(FieldDecl _ fnl tp) -> (fnl, tp)) (fieldsOf env cn) of
        Nothing -> tError (UnknownFieldName (getLoc e) fn cn)
        Just ft -> pure (eraseAnn ft)

    return $ FldAccE (setType annot tres) te fn

  TupleE annot es -> do
    tes <- traverse (tpExpr env) es
    let tres = TupleT () (map getTypeOfExpr tes)
    return $ TupleE (setType annot tres) tes

  CastE annot ctp e -> do
    te <- tpExpr env e
    let ctpEr = eraseAnn ctp
        t  = getTypeOfExpr te
    tres <- if castCompatible t ctpEr
               then pure ctpEr
               else tError (CastIncompatible [getLoc annot, getLoc e] t ctpEr)
    return $ CastE (setType annot tres) (addDummyTypes ctp) te

  _ -> error "typing of lists not implemented yet"

tpRule :: Environment t -> Rule Untyped -> TCEither (Rule Typed)
tpRule env (Rule ann rn instr vds precond postcond) = do
  tpdVds <- traverse (tpVarDecl env) vds
  let renv = pushLocalVarDecls vds env
  (teprecond, tepostcond)  <- (,) <$> tpExpr renv precond <*> tpExpr renv postcond
  -- These could be checked in "parallel" to get more errors at the same time
  _tprec <- checkBoolTp ann teprecond
  tres <- checkBoolTp ann tepostcond -- tpostcond == booleanT?
  return $ (\t -> Rule (setType ann t) rn instr tpdVds teprecond tepostcond) tres

tpAssertion :: Environment t -> Assertion Untyped -> TCEither (Assertion Typed)
tpAssertion env (Assertion ann nm md e) = do
  te <- tpExpr env e
  tres <- checkBoolTp ann te
  return $ Assertion (setType ann tres) nm md te


tpVarDecl :: Environment t -> VarDecl Untyped -> TCEither (VarDecl Typed)
tpVarDecl env (VarDecl ann vn tp) = do
  let kenv = map nameOfClassDecl (classDeclsOfEnv env)
  annotTp <- kndType kenv tp
  return $ (\t -> VarDecl (setType ann (eraseAnn t)) vn t) annotTp


----------------------------------------------------------------------
-- Class declaration errors


checkClassesForWfError :: HasLoc t => [ClassDecl t] -> p -> TCEither p
checkClassesForWfError cds prg =
  let class_names = map nameOfClassDecl cds
  in
    case filter (not . definedSuperclass class_names) cds of
      [] -> case duplicates class_names of
              [] -> pure prg
              ds -> tError (DuplicateClassNamesCDEErr [(getLoc cd, nameOfClassDecl cd) | cd <- cds, nameOfClassDecl cd `elem` ds])
      undefs -> tError (UndefinedSuperclassCDEErr (map (\cd -> (getLoc cd, nameOfClassDecl cd)) undefs))


-- ATTENTION, difference wrt. checkClassesForCyclicError: the first parameter is the list of prelude class decls and not the list of all class decls
-- TODO: In this function, the prelude class decs are prefixed to the program elements. 
checkClassesForCyclicError :: HasLoc t => [ClassDecl t] -> NewProgram t -> TCEither (NewProgram t)
checkClassesForCyclicError preludeCds prg =
  let cds = preludeCds ++ classDeclsOfNewProgram prg
      cdfAssoc = classDefAssoc cds
      cyclicClassNames = lefts (map (superClassesConstr cdfAssoc [] . nameOfClassDecl) cds)
  in case cyclicClassNames of
     []   -> let newProgElems = map ClassDeclTLE preludeCds ++ elementsOfNewProgram prg
                 elabSupers = map (mapClassDecl (elaborateSupersInClassDecl (superClasses cdfAssoc))) newProgElems
                 elabFields = map (mapClassDecl (elaborateFieldsInClassDecl (fieldAssoc cds))) elabSupers
             in pure (prg {elementsOfNewProgram = elabFields})
     cycs -> tError (CyclicClassHierarchyCDEErr
                [(getLoc cd, nameOfClassDecl cd)| cd <- cds, nameOfClassDecl cd `elem` cycs])

checkClassDeclsError :: HasLoc t => NewProgram t -> NewProgram t -> TCEither (NewProgram t)
checkClassDeclsError prelude prg =
  let pcds = classDeclsOfNewProgram prelude
      initialClassDecls = (pcds ++ classDeclsOfNewProgram prg)
  in do
    _ <- checkClassesForWfError initialClassDecls prg
    checkClassesForCyclicError pcds prg




----------------------------------------------------------------------
-- Field declaration errors

checkDuplicateFieldNamesFDE ::HasLoc t =>  NewProgram t -> TCEither (NewProgram t)
checkDuplicateFieldNamesFDE prg =
  let classDeclsWithDup = [cd | cd <- classDeclsOfNewProgram prg, not (null (duplicates (map nameOfFieldDecl ((fieldsOfClassDef . defOfClassDecl)  cd)))) ]
  in case classDeclsWithDup of
    [] -> pure prg
    cds -> tError (DuplicateFieldNamesFDEErr
              (map (\cd -> (getLoc cd, nameOfClassDecl cd,
                    map (\fd -> (getLoc fd, nameOfFieldDecl fd)) (duplicatesWrtFun nameOfFieldDecl (fieldsOfClassDef (defOfClassDecl cd)))))
               cds))

-- TODO: it would in principle be necessary to rewrite the types occurring in fields with KindT
-- (same for checkUndefinedTypeVDE)
checkUndefinedTypeFDE :: NewProgram Untyped -> TCEither (NewProgram Untyped)
checkUndefinedTypeFDE prg = do
  let kenv = map nameOfClassDecl (classDeclsOfNewProgram prg)
      fds = concatMap (fieldsOfClassDef . defOfClassDecl) (classDeclsOfNewProgram prg)
  _undefTps <- traverse (kndType kenv . tpOfFieldDecl) fds
  return prg

checkFieldDeclsError :: NewProgram Untyped -> TCEither (NewProgram Untyped)
checkFieldDeclsError prg =
  do
    _ <- checkDuplicateFieldNamesFDE prg
    checkUndefinedTypeFDE prg


----------------------------------------------------------------------
-- Global variable declaration errors

checkDuplicateVarNamesVDE :: HasLoc t => NewProgram t -> TCEither (NewProgram t)
checkDuplicateVarNamesVDE prg =
  case duplicatesWrtFun nameOfVarDecl (globalsOfNewProgram  prg) of
    [] -> pure prg
    vds -> tError (DuplicateVarNamesVDEErr (map (\vd -> (getLoc vd, nameOfVarDecl vd)) vds))

checkUndefinedTypeVDE :: NewProgram Untyped -> TCEither (NewProgram Untyped)
checkUndefinedTypeVDE prg = do
  let kenv = map nameOfClassDecl (classDeclsOfNewProgram prg)
  _undefTps <- traverse (kndType kenv . tpOfVarDecl) (globalsOfNewProgram prg)
  return prg

checkVarDeclsError :: NewProgram Untyped -> TCEither (NewProgram Untyped)
checkVarDeclsError prg =
  do
    _ <- checkDuplicateVarNamesVDE prg
    checkUndefinedTypeVDE prg


----------------------------------------------------------------------
-- Errors in Rules and Assertions

tpComponent :: Environment t-> TopLevelElement Untyped -> TCEither (TopLevelElement Typed)
tpComponent env e = case e of
  AssertionTLE c -> fmap AssertionTLE (tpAssertion env c)
  RuleTLE c -> fmap RuleTLE (tpRule env c)
  x -> pure $ addDummyTypes x

checkComponentsError :: NewProgram Untyped -> TCEither (NewProgram Typed)
checkComponentsError  prg = do
  let env = initialEnvOfProgram (classDeclsOfNewProgram prg) (globalsOfNewProgram prg)
  tpdComponents <- traverse (tpComponent env) (elementsOfNewProgram prg)
  return $ (addDummyTypes prg) {elementsOfNewProgram = tpdComponents}

----------------------------------------------------------------------
-- Summing up: all errors
-- the lifted version of the checkError function below
-- checkErrorGen :: (TypeAnnot f, HasLoc (f (Tp()))) => Program (f (Tp())) -> Program (f (Tp())) -> Either Error (Program (f (Tp())))
checkErrorLift :: NewProgram Untyped -> NewProgram Untyped -> TCEither (NewProgram Typed)
checkErrorLift prelude prg =
  do
    prgClsDecls <- checkClassDeclsError prelude prg
    _ <- checkFieldDeclsError prgClsDecls
    _ <- checkVarDeclsError prgClsDecls
    checkComponentsError prgClsDecls

type Untyped = SRng
type Typed = LocTypeAnnot (Tp ())

liftLoc :: Untyped -> LocTypeAnnot (Tp ())
liftLoc rng  = LocTypeAnnot rng OkT

-- | Since we don't have a distinction between type annotations and location annotations, we need to add dummy types ('OkT') in some nodes
addDummyTypes :: Functor f => f Untyped -> f Typed
addDummyTypes = fmap liftLoc

-- | Given the AST for the prelude and a module, return either a list of type errors or a typechecked module
-- TODO: remove "mapLeft ErrorCauseErr" to get a TCEither (NewProgram (LocTypeAnnot (Tp())))
checkError :: NewProgram Untyped -> NewProgram Untyped -> Either Error (NewProgram Typed)
checkError prelude prg = mapLeft ErrorCauseErr (getEither $ checkErrorLift prelude prg)


----------------------------------------------------------------------
-- Typing Timed Automata
----------------------------------------------------------------------
-- TOOD: the following section about Timed Automata is legacy code.
{-

tpCmd :: (TypeAnnot f, HasLoc (f a)) => Environment [ClassName] -> Cmd (f a) -> Cmd (f Tp)
tpCmd env (Skip annot) = Skip (setType annot OkT)
tpCmd env (VAssign annot v e) =
    let te = tpExpr env e
        t = getTypeOfExpr te
        tv = tpVar env (getLoc annot) v       -- TODO: the location is the one of the whole assingment and not of the variable alone
        restp = propagateError [t, tv]
                (if compatibleType env t tv
                 then OkT
                 else ErrT (IllTypedSubExpr [getLoc annot, getLoc e] [t] [ExpectedSubTpOf tv ])
                )
    in VAssign (setType annot restp) v te
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

----------------------------------------------------------------------
-- For the records
----------------------------------------------------------------------

{- The following is an alternative method of type checking. 
Contrary to the TCEither type used above, the result returned is not either an error or a typed expression,
but both. The advantages of such a type could be twofold:
(1) Even in the case of type errors, the parts of the syntax tree without errors could be explored and provide type information.
(2) The current algorithm no longer relies on lazy evaluation (e.g. the former frequent getTypeOfExpr (fromTRight ...)). This could be avoided
    with the TCResult type.
-}

{-
data TCResult r = TCResult {valOfTC :: r, errsOfTC :: [ErrorCause]}

propagateErrorTCResult :: [TCResult t1] -> TCResult t2 -> TCResult t2
propagateErrorTCResult trs tr =
  case concatMap errsOfTC trs of
    [] -> tr
    errs -> TCResult (valOfTC tr) errs

tpUarithTCResult :: Environment te -> [SRng] -> Tp t -> UArithOp -> TCResult (Tp t)
tpUarithTCResult env locs t ua =
  if isNumberTp env t
  then TCResult t []
  else TCResult t [IllTypedSubExpr locs  [eraseAnn t] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))]]

tpUboolTCResult :: [SRng] -> Tp t -> TCResult (Tp t)
tpUboolTCResult locs t =
  if isBooleanTp t
  then TCResult t []
  else TCResult t [IllTypedSubExpr locs [eraseAnn t] [ExpectedExactTp booleanT]]


tpUnaopTCResult :: Environment te -> [SRng] -> Tp t -> UnaOp -> TCResult (Tp t)
tpUnaopTCResult env locs t uop = case uop of
    UArith ua  -> tpUarithTCResult env locs t ua
    UBool _   -> tpUboolTCResult locs t
    UTemporal _ -> tpUboolTCResult locs t

tpVarTCResult :: Environment te -> SRng -> Var t -> TCResult (Tp ())
tpVarTCResult env loc (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in case lookup vn (globalsOfEnv env) of
      Nothing -> case lookup vn (localsOfEnv env)of
        Nothing -> TCResult ErrTWOCause [UndeclaredVariable loc vn]
        Just t -> TCResult t []
      Just t -> TCResult t []
tpVarTCResult env _ (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

liftresTCResult :: [TCResult e] -> TCResult t -> (t -> e) -> TCResult e
liftresTCResult resexp (TCResult tcval tcerr) f = TCResult (f tcval)  (tcerr ++ concatMap errsOfTC resexp)

tpExprTCResult :: Environment te -> Expr (LocTypeAnnot (Tp())) -> TCResult (Expr (LocTypeAnnot (Tp())))
tpExprTCResult env x = case x of
  ValE annot c -> TCResult (ValE (setType annot (tpConstval c)) c) []
  VarE annot v -> case tpVarTCResult env (getLoc annot) v of
    TCResult t [] -> TCResult (VarE (setType annot t) (varIdentityInEnv env v)) []
    -- in the error case, it is not clear whether v is in context and its identity can be determined
    TCResult t err -> TCResult (VarE (setType annot t) v) err

  UnaOpE annot uop e ->
    let te = tpExprTCResult env e
        tres  = tpUnaopTCResult env [getLoc annot, getLoc e] (getTypeOfExpr (valOfTC te)) uop
    in liftresTCResult [te] tres (\t -> UnaOpE (setType annot t) uop (valOfTC te))

  _ -> undefined

-}