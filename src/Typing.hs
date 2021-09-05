-- Typing of expressions
{-
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-}

{-# LANGUAGE TypeFamilies #-}

module Typing where

import Data.List ( elemIndex, nub )
import Data.Maybe ( fromMaybe )
import Data.Either (isLeft, lefts)
import Data.Either.Combinators (mapLeft, mapRight, fromRight')
import Data.List.Utils ( countElem )


import Annotation
    ( LocTypeAnnot(LocTypeAnnot, typeAnnot), SRng, TypeAnnot(..), HasLoc(..) )
import Error
import Syntax
import GHC.Exception (ErrorCall)
import Control.Monad (join)
import Control.Applicative ((<|>))




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

{-
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
-}

{-
propagateError :: [Tp t] -> Tp t -> Tp t
propagateError ts t =
  if any isErrTp ts
  then ErrT Inherited
  else t
-}

-- simple
-- TODO: propagate error information, turn into a function with result type TCEither
kndTypeS :: KindEnvironment -> Tp (LocTypeAnnot (Tp ())) -> Tp (LocTypeAnnot (Tp ()))
kndTypeS kenv (ClassT ann cn) =
  if cn `elem` kenv
  then ClassT (updType ann KindT) cn
  else ClassT (updType ann (ErrT )) cn
  -- else ClassT (updType ann (ErrT UndefinedTypeInClassT)) cn
kndTypeS kenv (FunT ann a b) =
  let aI = kndTypeS kenv a
      bI = kndTypeS kenv b
      -- resT = propagateError (map (typeAnnot . annotOfTp) [aI, bI]) KindT
      resT = KindT
  in FunT (updType ann resT) aI bI
kndTypeS kenv (TupleT ann ts) =
  let subTs = map (kndTypeS kenv) ts
      -- resT = propagateError (map (typeAnnot . annotOfTp) subTs) KindT
      resT = KindT
  in TupleT (updType ann resT) subTs
kndTypeS kenv (ErrT ) = ErrT
kndTypeS kenv OkT = OkT
kndTypeS kenv KindT = KindT


----------------------------------------------------------------------
-- Linking classes from the prelude to internal predicates
----------------------------------------------------------------------


checkSimpleTp :: String -> Tp t -> TCEither ()
checkSimpleTp _ _ = undefined

checkBooleanTp = checkSimpleTp "Boolean"

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

type TCEither t = Either [ErrorCause] t

-- newtype TCEither t = TCEither (Either [ErrorCause] t)


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

tpUarith :: Environment te -> [SRng] -> Tp t -> UArithOp -> TCEither (Tp t)
tpUarith env locs t _ua =
  if isNumberTp env t
  then Right t
  else Left [IllTypedSubExpr locs  [eraseAnn t] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))]]

-- applicable to both unary boolean as temporal modal operators
tpUbool :: [SRng] -> Tp t -> TCEither (Tp t)
tpUbool locs t =
  if isBooleanTp t
  then Right t
  else Left [IllTypedSubExpr locs [eraseAnn t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]

tpUnaop :: Environment te -> [SRng] -> TCEither (Tp t) -> UnaOp -> TCEither (Tp t)
tpUnaop env locs (Right t) uop = case uop of
    UArith ua  -> tpUarith env locs t ua
    UBool _   -> tpUbool locs t
    UTemporal _ -> tpUbool locs t
tpUnaop _env _locs _ _uop = Left []

-- TODO: The error message is inappropriate. Change when reworking the type checking algorithm
tpTime :: [SRng] -> Tp () -> Tp () -> BArithOp -> TCEither (Tp ())
tpTime _ _ _  BAadd = Right (ClassT () (ClsNm "Time"))
tpTime _ _ _  BAsub = Right (ClassT () (ClsNm "Time"))
tpTime locs t1 t2 _ = Left [IllTypedSubExpr [locs!!0,locs!!1] [t1] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))]]

tpBarith :: Environment te -> [SRng] -> Tp () -> Tp () -> BArithOp -> TCEither (Tp ())
tpBarith env locs t1 t2 ba =
  if isNumberTp env t1
  then if isNumberTp env t2
       then
         if isTimeTp t1 || isTimeTp t2
         then tpTime locs t1 t2 ba
         else Right (leastCommonSuperType env t1 t2)
       else Left [IllTypedSubExpr [locs!!0,locs!!2] [t2] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))]]
  else Left [IllTypedSubExpr [locs!!0,locs!!1] [t1] [ExpectedSubTpOf (ClassT () (ClsNm "Number"))]]

-- TODO: more liberal condition for comparison?
tpBcompar :: Environment te -> [SRng] -> Tp () -> Tp () -> BComparOp -> TCEither (Tp ())
tpBcompar env locs t1 t2 _bc =
  if isScalarTp t1 && isScalarTp t2
  then if compatibleType env t1 t2 || compatibleType env t2 t1
       then Right booleanT
       else Left [IncompatibleTp locs [t1, t2]]
  else Left [NonScalarExpr locs [t1, t2]]

tpBbool :: Environment te -> [SRng] -> Tp () -> Tp () -> BBoolOp -> TCEither (Tp ())
tpBbool _env locs t1 t2 _bc =
  if isBooleanTp t1
  then if isBooleanTp t2
       then Right booleanT
       else Left [IllTypedSubExpr [locs!!0,locs!!2] [t2] [ExpectedExactTp booleanT]]
  else Left [IllTypedSubExpr [locs!!0,locs!!1] [t1] [ExpectedExactTp booleanT]]


tpBinop :: Environment te -> [SRng] -> TCEither (Tp ()) -> TCEither (Tp ()) -> BinOp -> TCEither (Tp ())
tpBinop env locs (Right t1) (Right t2) bop = case bop of
    BArith ba  -> tpBarith env locs t1 t2 ba
    BCompar bc -> tpBcompar env locs t1 t2 bc
    BBool bb   -> tpBbool env locs t1 t2 bb
tpBinop _env _locs _ _ _bop = Left []



-- the first type can be cast to the second type
-- TODO: still to be defined
castCompatible :: Tp t -> Tp t -> Bool
castCompatible te ctp = True


-- typing of a variable that is initially (after parsing) only known by its name
tpVar :: Environment te -> SRng -> Var t -> TCEither (Tp ())
tpVar env loc (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in case lookup vn (globalsOfEnv env) of
      Nothing -> case lookup vn (localsOfEnv env)of
        Nothing -> Left [UndeclaredVariable loc vn]
        Just t -> Right t
      Just t -> Right t
tpVar env _ (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

varIdentityInEnv :: Environment te -> Var t -> Var t
varIdentityInEnv (Env _ _ vds) (GlobalVar qvn) =
  let vn = nameOfQVarName qvn
  in maybe (GlobalVar qvn) (LocalVar qvn) (elemIndex vn (map fst vds))
varIdentityInEnv env (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"

pushLocalVarEnv :: VarEnvironment -> Environment t -> Environment t
pushLocalVarEnv nvds (Env cls gv vds) = Env cls gv (reverse nvds ++ vds)

pushLocalVarDecl :: VarDecl t1 -> Environment t2 -> Environment t2
pushLocalVarDecl v = pushLocalVarEnv [(nameOfVarDecl v, eraseAnn (tpOfVarDecl v))]

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


liftresTCEither :: [TCEither e] -> (t -> a) -> TCEither t -> TCEither a
liftresTCEither resexps f restp =
  case concat (lefts resexps) of
    [] -> case restp of
            Right t -> Right (f t)
            Left x -> Left x
    errs -> Left errs

-- TODO: check well-formedness of types in function abstraction and in quantification
tpExpr :: Environment te -> Expr (LocTypeAnnot (Tp())) -> TCEither (Expr (LocTypeAnnot (Tp())))
tpExpr env expr = case expr of
  ValE annot c -> Right (ValE (updType annot (tpConstval c)) c)
  VarE annot v -> case tpVar env (getLoc annot) v of
    Right t -> Right (VarE (updType annot t) (varIdentityInEnv env v))
    Left errs -> Left errs

  UnaOpE annot uop e ->
    let te = tpExpr env e
        tres  = tpUnaop env [getLoc annot, getLoc e] (mapRight getTypeOfExpr te) uop
    in liftresTCEither [te] (\t -> UnaOpE (updType annot t) uop (fromRight' te)) tres

  BinOpE annot bop e1 e2 ->
    let te1 = tpExpr env e1
        te2 = tpExpr env e2
        tres   = tpBinop env [getLoc annot, getLoc e1, getLoc e2] (mapRight getTypeOfExpr te1) (mapRight getTypeOfExpr te2) bop
    in  liftresTCEither [te1, te2] (\t -> BinOpE (updType annot t) bop (fromRight' te1) (fromRight' te2)) tres

  -- TODO: consider a more liberal typing returning the least common supertype of the two branches
  IfThenElseE annot ec e1 e2 ->
    let tec = tpExpr env ec
        te1 = tpExpr env e1
        te2 = tpExpr env e2
        tc = getTypeOfExpr (fromRight' tec)
        t1 = getTypeOfExpr (fromRight' te1)
        t2 = getTypeOfExpr (fromRight' te2)
        tres = if isBooleanTp tc
               then if compatibleType env t1 t2
                    then Right t2
                    else if compatibleType env t2 t1
                         then Right t1
                         else Left [IncompatibleTp [getLoc annot, getLoc e1, getLoc e2] [t1, t2]]
               else Left [IllTypedSubExpr [getLoc annot, getLoc ec] [tc] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]
    in liftresTCEither [tec, te1, te2] (\t -> IfThenElseE (updType annot t) (fromRight' tec) (fromRight' te1) (fromRight' te2)) tres

{- 
-- Using Applicative and Alternative instead
  IfThenElseE annot ec e1 e2 ->
    let tec = tpExpr env ec
        te1 = tpExpr env e1
        te2 = tpExpr env e2

        guardMsg _ True = Right ()
        guardMsg e False = Left e

        incompatMsg t1 t2 = [IncompatibleTp [getLoc annot, getLoc e1, getLoc e2] [t1, t2]]
        notBoolMsg tc = [IllTypedSubExpr [getLoc annot, getLoc ec] [tc] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]
        checkBooleanType t = guardMsg (notBoolMsg t) (isBooleanTp t)
        checkCompatLeft t1 t2 = t2 <$ guardMsg (incompatMsg t1 t2) (compatibleType env t1 t2)
        checkCompat t1 t2 = checkCompatLeft t1 t2 <|> checkCompatLeft t2 t1

        getResult tc' t1' t2' = let
          tc = getTypeOfExpr tc'
          t1 = getTypeOfExpr t1'
          t2 = getTypeOfExpr t2'
          in do
            t <- tres tc t1 t2
            return $ IfThenElseE (updType annot t) (fromRight' tec) (fromRight' te1) (fromRight' te2)
        tres tc t1 t2 = do
          checkBooleanType tc
          checkCompat t1 t2
    in join $ getResult <$> tec <*> te1 <*> te2
-}

  AppE annot fe ae ->
    let tfe = tpExpr env fe
        tae = tpExpr env ae
        tf  = getTypeOfExpr (fromRight' tfe)
        ta  = getTypeOfExpr (fromRight' tae)
        tres = (case tf of
                      FunT _ tpar tbody ->
                        if compatibleType env ta tpar
                        then Right tbody
                        else Left [IllTypedSubExpr [getLoc annot, getLoc ae] [ta] [ExpectedSubTpOf tpar]]
                      _ -> Left [NonFunctionTp [getLoc annot, getLoc fe] tf])
    in liftresTCEither [tfe, tae] (\t -> AppE (updType annot t) (fromRight' tfe) (fromRight' tae)) tres

  FunE annot pt tparam e ->
    let te = tpExpr (pushPatternEnv pt tparam env) e
        t  = getTypeOfExpr (fromRight' te)
        tres = if compatiblePatternType pt tparam
               then Right (FunT () (eraseAnn tparam) t)
               else Left [IncompatiblePattern (getLoc annot)]
    in liftresTCEither [te] (\tl -> FunE (updType annot tl) pt tparam (fromRight' te)) tres

  QuantifE annot q v e ->
    let te = tpExpr (pushLocalVarDecl v env) e
        t  = getTypeOfExpr (fromRight' te)
        tres = if isBooleanTp t
               then Right booleanT
               else Left [IllTypedSubExpr [getLoc annot, getLoc e] [t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]
    in liftresTCEither [te] (\tl -> QuantifE (updType annot tl) q v (fromRight' te)) tres

  FldAccE annot e fn ->
    let te = tpExpr env e
        t  = getTypeOfExpr (fromRight' te)
        tres = (case t of
                      ClassT _ cn ->
                         case lookup fn (map (\(FieldDecl _ fnl tp) -> (fnl, tp)) (fieldsOf env cn)) of
                           Nothing -> Left [UnknownFieldName (getLoc e) fn cn]
                           Just ft -> Right (eraseAnn ft)
                      _ -> Left [AccessToNonObjectType (getLoc e)]
                  )
    in liftresTCEither [te] (\tl -> FldAccE (updType annot tl) (fromRight' te) fn) tres

  TupleE annot es ->
    let tes = map (tpExpr env) es
    in liftresTCEither tes (\t -> TupleE (updType annot t) (map fromRight' tes)) (Right (TupleT () (map (getTypeOfExpr . fromRight') tes)))

  CastE annot ctp e ->
    let te = tpExpr env e
        ctpEr = eraseAnn ctp
        t  = getTypeOfExpr (fromRight' te)
        tres = if castCompatible t ctpEr
               then Right ctpEr
               else Left [CastIncompatible [getLoc annot, getLoc e] t ctpEr]
    in liftresTCEither [te] (\tl -> CastE (updType annot tl) ctp (fromRight' te)) tres

  _ -> error "typing of lists not implemented yet"

-- TODO: check types of variable declarations (possible well-formedness problems currently not taken into account)
tpRule :: Environment t -> Rule (LocTypeAnnot (Tp ())) -> TCEither (Rule (LocTypeAnnot (Tp ())))
tpRule env (Rule ann rn instr vds precond postcond) =
  let renv = pushLocalVarEnv (map (\(VarDecl _ vn vt) -> (vn, eraseAnn vt)) vds) env
      teprecond  = tpExpr renv precond
      tepostcond = tpExpr renv postcond
      tprecond = getTypeOfExpr (fromRight' teprecond)
      tpostcond = getTypeOfExpr (fromRight' tepostcond)
      kenv = map nameOfClassDecl (classDeclsOfEnv env)
      tpdVds = map (tpVarDecl kenv) vds
      tres =  if isBooleanTp tprecond
              then if isBooleanTp tpostcond
                   then Right tpostcond
                   else Left [IllTypedSubExpr [getLoc ann, getLoc postcond] [tpostcond] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]
              else Left [IllTypedSubExpr [getLoc ann, getLoc precond] [tprecond] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]
  in liftresTCEither[teprecond, tepostcond] (\t -> Rule (updType ann t) rn instr tpdVds (fromRight' teprecond) (fromRight' tepostcond)) tres

tpAssertion :: Environment t -> Assertion (LocTypeAnnot (Tp ())) -> TCEither (Assertion (LocTypeAnnot (Tp ())))
tpAssertion env (Assertion ann nm md e) =
  let te = tpExpr env e
      t  = getTypeOfExpr (fromRight' te)
      tres =  (if isBooleanTp t
               then Right t
               else Left [IllTypedSubExpr [getLoc ann, getLoc e] [t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]])
  in liftresTCEither [te] (\tl -> Assertion (updType ann tl) nm md (fromRight' te)) tres


tpVarDecl :: [ClassName] -> VarDecl (LocTypeAnnot (Tp ()))  -> VarDecl (LocTypeAnnot (Tp ()))
tpVarDecl kenv (VarDecl ann vn tp) =
  let annotTp = kndTypeS kenv tp
  in  VarDecl (updType ann (eraseAnn annotTp)) vn annotTp


----------------------------------------------------------------------
-- Class declaration errors


checkClassesForWfError :: HasLoc t => [ClassDecl t] -> p -> TCEither p
checkClassesForWfError cds prg =
  let class_names = map nameOfClassDecl cds
  in
    case filter (not . definedSuperclass class_names) cds of
      [] -> case duplicates class_names of
              [] -> Right prg
              ds -> Left [DuplicateClassNamesCDEErr
                    [(getLoc cd, nameOfClassDecl cd) | cd <- cds, nameOfClassDecl cd `elem` ds]]
      undefs -> Left [UndefinedSuperclassCDEErr (map (\cd -> (getLoc cd, nameOfClassDecl cd)) undefs)]


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
             in Right (prg {elementsOfNewProgram = elabFields})
     cycs -> Left [CyclicClassHierarchyCDEErr
                     [(getLoc cd, nameOfClassDecl cd)| cd <- cds, nameOfClassDecl cd `elem` cycs]]

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
    [] -> Right prg
    cds -> Left [DuplicateFieldNamesFDEErr
                  (map (\cd -> (getLoc cd, nameOfClassDecl cd,
                        map (\fd -> (getLoc fd, nameOfFieldDecl fd)) (duplicatesWrtFun nameOfFieldDecl (fieldsOfClassDef (defOfClassDecl cd)))))
                   cds)]

checkUndefinedTypeFDE :: HasLoc t => NewProgram t -> TCEither (NewProgram t)
checkUndefinedTypeFDE prg =
  let kenv = map nameOfClassDecl (classDeclsOfNewProgram prg)
      classDeclsWithUndefTp = [cd | cd <- classDeclsOfNewProgram prg, not (null (lefts (map (kndType kenv . tpOfFieldDecl) (fieldsOfClassDef (defOfClassDecl cd)))))]
  in case classDeclsWithUndefTp of
    [] -> Right prg
    cds -> Left [UndefinedTypeFDEErr (concatMap (\cd -> [(getLoc fd, nameOfFieldDecl fd) | fd <- fieldsOfClassDef (defOfClassDecl cd), isLeft (kndType kenv (tpOfFieldDecl fd))  ]) cds)]

checkFieldDeclsError ::HasLoc t =>  NewProgram t -> TCEither (NewProgram t)
checkFieldDeclsError prg =
  do
    _ <- checkDuplicateFieldNamesFDE prg
    checkUndefinedTypeFDE prg


----------------------------------------------------------------------
-- Global variable declaration errors

checkDuplicateVarNamesVDE :: HasLoc t => NewProgram t -> TCEither (NewProgram t)
checkDuplicateVarNamesVDE prg =
  case duplicatesWrtFun nameOfVarDecl (globalsOfNewProgram  prg) of
    [] -> Right prg
    vds -> Left [DuplicateVarNamesVDEErr (map (\vd -> (getLoc vd, nameOfVarDecl vd)) vds)]

checkUndefinedTypeVDE :: HasLoc t => NewProgram t -> TCEither (NewProgram t)
checkUndefinedTypeVDE prg =
  let kenv = map nameOfClassDecl (classDeclsOfNewProgram prg)
      varDeclsWithUndefTp = [vd | vd <- globalsOfNewProgram prg, isLeft (kndType kenv (tpOfVarDecl vd))]
  in case varDeclsWithUndefTp of
    [] -> Right prg
    vds -> Left [UndefinedTypeVDEErr (map (\vd -> (getLoc vd, nameOfVarDecl vd)) vds)]

checkVarDeclsError :: HasLoc t => NewProgram t -> TCEither (NewProgram t)
checkVarDeclsError prg =
  do
    _ <- checkDuplicateVarNamesVDE prg
    checkUndefinedTypeVDE prg


----------------------------------------------------------------------
-- Errors in Rules and Assertions

tpComponent :: Environment t-> TopLevelElement (LocTypeAnnot (Tp ())) -> TCEither (TopLevelElement (LocTypeAnnot (Tp ())))
tpComponent env e = case e of
  AssertionTLE c -> mapRight AssertionTLE (tpAssertion env c)
  RuleTLE c -> mapRight RuleTLE (tpRule env c)
  x -> Right x

checkComponentsError :: NewProgram (LocTypeAnnot (Tp ())) -> TCEither (NewProgram (LocTypeAnnot (Tp ())))
checkComponentsError  prg =
  let env = initialEnvOfProgram (classDeclsOfNewProgram prg) (globalsOfNewProgram prg)
      tpdComponents = map (tpComponent env) (elementsOfNewProgram prg)
  in case lefts tpdComponents of
   [] -> Right (prg {elementsOfNewProgram = map fromRight' tpdComponents})
   errs -> Left (concat errs)

----------------------------------------------------------------------
-- Summing up: all errors
-- the lifted version of the checkError function below
-- checkErrorGen :: (TypeAnnot f, HasLoc (f (Tp()))) => Program (f (Tp())) -> Program (f (Tp())) -> Either Error (Program (f (Tp())))
checkErrorLift :: NewProgram (LocTypeAnnot (Tp ())) -> NewProgram (LocTypeAnnot (Tp ())) -> TCEither (NewProgram (LocTypeAnnot (Tp ())))
checkErrorLift prelude prg =
  do
    prgClsDecls <- checkClassDeclsError prelude prg
    _ <- checkFieldDeclsError prgClsDecls
    _ <- checkVarDeclsError prgClsDecls
    checkComponentsError prgClsDecls

liftLoc :: SRng -> LocTypeAnnot (Tp ())
liftLoc rng  = LocTypeAnnot rng OkT

-- TODO: remove "mapLeft ErrorCauseErr" to get a TCEither (NewProgram (LocTypeAnnot (Tp())))
checkError :: NewProgram SRng -> NewProgram SRng -> Either Error (NewProgram (LocTypeAnnot (Tp())))
checkError prelude prg = mapLeft ErrorCauseErr (checkErrorLift (fmap liftLoc prelude) (fmap liftLoc prg))


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

----------------------------------------------------------------------
-- For the records
----------------------------------------------------------------------

{- The following is an alternative method of type checking. 
Contrary to the TCEither type used above, the result returned is not either an error or a typed expression,
but both. The advantages of such a type could be twofold:
(1) Even in the case of type errors, the parts of the syntax tree without errors could be explored and provide type information.
(2) The current algorithm relies on lazy evaluation (e.g. the frequent getTypeOfExpr (fromRight' ...)). This could be avoided
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
  else TCResult t [IllTypedSubExpr locs [eraseAnn t] [ExpectedExactTp (ClassT () (ClsNm "Boolean"))]]


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
  ValE annot c -> TCResult (ValE (updType annot (tpConstval c)) c) []
  VarE annot v -> case tpVarTCResult env (getLoc annot) v of
    TCResult t [] -> TCResult (VarE (updType annot t) (varIdentityInEnv env v)) []
    -- in the error case, it is not clear whether v is in context and its identity can be determined
    TCResult t err -> TCResult (VarE (updType annot t) v) err

  UnaOpE annot uop e ->
    let te = tpExprTCResult env e
        tres  = tpUnaopTCResult env [getLoc annot, getLoc e] (getTypeOfExpr (valOfTC te)) uop
    in liftresTCResult [te] tres (\t -> UnaOpE (updType annot t) uop (valOfTC te))

  _ -> undefined

-}