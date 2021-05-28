
module Error where
import Syntax
import Annotation (RealSRng(..), SRng(..), Pos(Pos) )

data ClassDeclsError
    = DuplicateClassNamesCDE [(SRng, ClassName)]  -- classes whose name is defined multiple times
    | UndefinedSuperclassCDE [(SRng, ClassName)]  -- classes having undefined superclasses
    | CyclicClassHierarchyCDE [(SRng, ClassName)]  -- classes involved in a cyclic class definition
  deriving (Eq, Ord, Show, Read)

data FieldDeclsError
    = DuplicateFieldNamesFDE [(SRng, ClassName, [(SRng, FieldName)])]     -- field names with duplicate defs
    | UndefinedTypeFDE [(SRng, FieldName)]              -- field names with duplicate defs
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

data Error
    = ClassDeclsErr ClassDeclsError
    | FieldDeclsErr FieldDeclsError
    | VarDeclsErr VarDeclsError
    | RuleErr RuleError
    | AssertionErr AssertionError
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

printTp :: Tp -> String
printTp t = case t of
  ClassT (ClsNm "Boolean") -> "Bool"
  ClassT (ClsNm "Integer") -> "Integer"
  ClassT cn -> printClassName cn
  FunT t1 t2 -> "(" ++ printTp t1 ++ " -> " ++ printTp t2 ++")"
  TupleT [] -> "()"
  TupleT [t] -> "(" ++ printTp t ++ ")"
  TupleT (t:ts) -> "(" ++ printTp t ++ ", " ++ (foldr (\s r -> ((printTp s) ++ ", " ++ r)) "" ts) ++ ")"
  _ -> error "internal error in printTp: ErrT or OkT not printable"

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
printErrorCause (UndeclaredVariable r vn) = "Variable " ++ printVarName vn ++ " at " ++ (printSRng r) ++ " is undefined."
printErrorCause (IllTypedSubExpr rngs givents expts) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines
  (map (\(r, gv, exp) -> "the subexpression at " ++ (printSRng r) ++ " has type " ++ (printTp gv) ++ " but " ++ (printExpectedTp exp) ++  " was expected")
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
printErrorCause c = show c

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
  unlines (map (\(r, ec) -> "In assertion:\n" ++ printErrorCause ec) illTyped)

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



