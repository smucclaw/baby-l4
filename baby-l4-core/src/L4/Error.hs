{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module L4.Error where
import Data.Data (Data, Typeable)
import L4.Syntax
import L4.Annotation (RealSRng(..), SRng(..), Pos(Pos) )
import PrintProg (printTp)

data ExpectedType
  = ExpectedString  String
  | ExpectedExactTp (Tp())
  | ExpectedSubTpOf (Tp())
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data ErrorCause
  = UndefinedClassInType SRng ClassName
  | UndeclaredVariable SRng VarName
  | IllTypedSubExpr { exprRangesITSE :: [SRng]    -- operators that require specific types  for their arguments
                    , receivedITSE :: [Tp()]
                    , expectedITSE :: [ExpectedType] }
  | IncompatibleTp { exprRangesITSE :: [SRng]     -- when we need two types to be the same for an operation, or perhaps a subtype (to check)
                    , receivedITSE :: [Tp()] }
  | NonScalarExpr { exprRangesITSE :: [SRng]      -- functions are not scalar types and not comparable
                    , receivedITSE :: [Tp()] }
  | NonFunctionTp { exprRangesITSE :: [SRng] -- call function when not function
                    , receivedFunTpITSE :: Tp() }
  | CastIncompatible { exprRangesITSE :: [SRng] -- typecasting from int to string for example (and its not compatible)
                    , receivedCastITSE :: Tp()
                    , castToITSE :: Tp() }
    -- TODO: the following has become obsolete
  | IncompatiblePattern SRng          -- pattern matching failure for tuples (l4)
  | UnknownFieldName SRng FieldName ClassName   -- class has no such field
  | AccessToNonObjectType SRng  -- when using dot notation on something thats not an object

  -- Previously spread out in several other types
  | DuplicateClassNamesCDEErr [(SRng, ClassName)]  -- classes whose name is defined multiple times
  | UndefinedSuperclassCDEErr [(SRng, ClassName)]  -- classes having undefined superclasses
  | CyclicClassHierarchyCDEErr [(SRng, ClassName)]  -- classes involved in a cyclic class definition

  | DuplicateFieldNamesFDEErr [(SRng, ClassName, [(SRng, FieldName)])]     -- field names with duplicate defs
    -- TODO: the following has become obsolete
  | UndefinedTypeFDEErr [(SRng, FieldName)]                                -- field names containing undefined types

  | DuplicateVarNamesVDEErr [(SRng, VarName)]
    -- TODO: the following has become obsolete
  | UndefinedTypeVDEErr [(SRng, VarName)]
  deriving (Eq, Ord, Show, Read, Data, Typeable)

{-# DEPRECATED IncompatiblePattern "Tuple patterns are no longer supported" #-}
{-# DEPRECATED UndefinedTypeFDEErr "This is now obsolete" #-}
{-# DEPRECATED UndefinedTypeVDEErr "This is now obsolete" #-}

data ClassDeclsError
    = DuplicateClassNamesCDE [(SRng, ClassName)]  -- classes whose name is defined multiple times
    | UndefinedSuperclassCDE [(SRng, ClassName)]  -- classes having undefined superclasses
    | CyclicClassHierarchyCDE [(SRng, ClassName)]  -- classes involved in a cyclic class definition
  deriving (Eq, Ord, Show, Read)

data FieldDeclsError
    = DuplicateFieldNamesFDE [(SRng, ClassName, [(SRng, FieldName)])]     -- field names with duplicate defs
    | UndefinedTypeFDE [(SRng, FieldName)]                                -- field names containing undefined types
  deriving (Eq, Ord, Show, Read)

data VarDeclsError
    = DuplicateVarNamesVDE [(SRng, VarName)]
    | UndefinedTypeVDE [(SRng, VarName)]
  deriving (Eq, Ord, Show, Read)

data RuleError
  = RuleErrorRE [(SRng, ErrorCause)]
  deriving (Eq, Ord, Show, Read)

data AssertionError
  = AssertionErrAE [(SRng, ErrorCause)]
  deriving (Eq, Ord, Show, Read)

-- TODO: after restructuring, only ErrorCauseErr remain. 
-- It will then be possible to remove the Error type altogether, and also the types
-- ClassDeclsError, FieldDeclsError, VarDeclsError, RuleError, AssertionError
data Error
    = ClassDeclsErr ClassDeclsError
    | FieldDeclsErr FieldDeclsError
    | VarDeclsErr VarDeclsError
    | RuleErr RuleError
    | AssertionErr AssertionError
    | ErrorCauseErr [ErrorCause]
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------------
-- Printing parts of the syntax
----------------------------------------------------------------------

printClassName :: ClassName -> String
printClassName (ClsNm cn) = cn

printFieldName :: FieldName -> String
printFieldName (FldNm fn) = fn

printVarName :: VarName -> String
printVarName = id

printExpectedTp :: ExpectedType -> String
printExpectedTp (ExpectedString s) = s
printExpectedTp (ExpectedExactTp t) = "the type " ++ printTp t
printExpectedTp (ExpectedSubTpOf t) = "a subtype of " ++ printTp t

----------------------------------------------------------------------
-- Printing errors
----------------------------------------------------------------------

printPos :: Pos -> String
printPos (Pos l c) = "(" ++ show l ++ "," ++ show c ++ ")"


printSRng :: SRng -> String
--printSRng = show
printSRng (RealSRng(SRng spos epos)) = printPos spos ++ " .. " ++ printPos epos
printSRng (DummySRng reason) = show reason

printErrorCause :: ErrorCause -> String
printErrorCause (UndefinedClassInType r cn) = "Class name " ++ printClassName cn ++ " at " ++ (printSRng r) ++ " is undefined."
printErrorCause (UndeclaredVariable r vn) = "Variable " ++ printVarName vn ++ " at " ++ (printSRng r) ++ " is undefined."
printErrorCause (IllTypedSubExpr rngs givents expts) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines
  (map (\(r, gv, expr) -> "the subexpression at " ++ (printSRng r) ++ " has type " ++ (printTp gv) ++ " but " ++ (printExpectedTp expr) ++  " was expected")
  (zip3 (tail rngs) givents expts))
printErrorCause (IncompatibleTp rngs givents) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines
  (map (\(r, gv) -> "the subexpression at " ++ (printSRng r) ++ " has type " ++ (printTp gv))
  (zip (tail rngs) givents)) ++
  "The types are not compatible (one is subtype of the other)"
printErrorCause (NonScalarExpr rngs givents) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines
  (map (\(r, gv) -> "the subexpression at " ++ (printSRng r) ++ " has type " ++ (printTp gv))
  (zip (tail rngs) givents)) ++
  "At least one type is not scalar (non-functional)"
printErrorCause (NonFunctionTp rngs giventp) =
  "Expression (application) at " ++ printSRng (head rngs) ++ " is ill-typed:\n" ++
  "the subexpression at " ++ printSRng (rngs!!1) ++ " has type " ++ printTp giventp ++ " which is not a functional type."
printErrorCause (CastIncompatible rngs giventp casttp) =
  "Expression at " ++ printSRng (head rngs) ++ " is ill-typed:\n" ++
  "the subexpression at "++ printSRng (rngs!!1) ++ " has type " ++ printTp giventp ++ "which cannot be cast to " ++ printTp casttp
printErrorCause (IncompatiblePattern r) =
    "Expression at "++ printSRng r ++ " is ill-typed: the variable pattern and its type are incompatible (different number of components)"
printErrorCause (UnknownFieldName r fn cn) =
    "Expression at "++ printSRng r ++ " is ill-typed: access to an unknown field " ++ printFieldName fn ++ " in class " ++ printClassName cn
printErrorCause (AccessToNonObjectType r) =
  "Expression at "++ printSRng r ++ " is ill-typed: access to a field of a non-object type\n"
printErrorCause (DuplicateClassNamesCDEErr cls) = printClassDeclsError $ DuplicateClassNamesCDE cls -- TODO: Inline these definitions
printErrorCause (UndefinedSuperclassCDEErr cls) = printClassDeclsError $ UndefinedSuperclassCDE cls
printErrorCause (CyclicClassHierarchyCDEErr cls) = printClassDeclsError $ CyclicClassHierarchyCDE cls
printErrorCause (DuplicateFieldNamesFDEErr dfs) = printFieldDeclsError $ DuplicateFieldNamesFDE dfs
printErrorCause (UndefinedTypeFDEErr dfs) = printFieldDeclsError $ UndefinedTypeFDE dfs
printErrorCause (DuplicateVarNamesVDEErr vdloc) = printVarDeclsError $ DuplicateVarNamesVDE vdloc
printErrorCause (UndefinedTypeVDEErr vdloc) = printVarDeclsError $ UndefinedTypeVDE vdloc

printClassLocName :: (SRng, ClassName) -> String
printClassLocName (r, cn) = "At " ++ printSRng r ++ " class name " ++ printClassName cn

printFieldLocName :: (SRng, FieldName) -> String
printFieldLocName (r, fn) = "At " ++ printSRng r ++ " field name " ++ printFieldName fn

printVarLocName :: (SRng, VarName) -> String
printVarLocName (r, vn) = "At " ++ printSRng r ++ " variable name " ++ printVarName vn


printClassDeclsError :: ClassDeclsError -> String
printClassDeclsError (DuplicateClassNamesCDE cls) =
  unlines ("Duplicate names in class declarations:" : map printClassLocName cls)
printClassDeclsError (UndefinedSuperclassCDE cls) =
  unlines ("Undefined superclasses in the following class declarations:" : map printClassLocName cls)
printClassDeclsError (CyclicClassHierarchyCDE cls) =
  unlines ("Cyclic class references in the following class declarations:" : map printClassLocName cls)


printFieldDeclsError :: FieldDeclsError -> String
printFieldDeclsError (DuplicateFieldNamesFDE dfs) =
  unlines ("Duplicate field names in the following classes:" :
              (map (\(r, cn, fdes) -> unlines (printClassLocName (r, cn) : map printFieldLocName fdes)) dfs) )
printFieldDeclsError (UndefinedTypeFDE fdloc) =
  unlines ("Undefined type in field declaration: " : map printFieldLocName fdloc)

printVarDeclsError :: VarDeclsError -> String
printVarDeclsError (DuplicateVarNamesVDE vdloc) =
  unlines ("Duplicate variable declarations: " : map printVarLocName vdloc)
printVarDeclsError (UndefinedTypeVDE vdloc) =
  unlines ("Undefined type in variable declaration: " : map printVarLocName vdloc)

printAssertionErr :: AssertionError -> String
printAssertionErr (AssertionErrAE illTyped) =
  unlines (map (\(_, ec) -> "In assertion:\n" ++ printErrorCause ec) illTyped)

printRuleErr :: RuleError -> String
printRuleErr (RuleErrorRE illTyped) =
  unlines (map (\(r, ec) -> "In rule " ++ printSRng r ++ "\n" ++ printErrorCause ec) illTyped)

printError :: Error -> String
printError e = case e of
  ClassDeclsErr cde -> printClassDeclsError cde
  FieldDeclsErr fde -> printFieldDeclsError fde
  VarDeclsErr vde -> printVarDeclsError vde
  RuleErr re -> printRuleErr re
  AssertionErr ae -> printAssertionErr ae
  ErrorCauseErr ecs -> unlines (map printErrorCause ecs)



