
module Error where
import Syntax
import Annotation ( SRng(SRng), Pos(Pos) )

data ClassDeclsError t
    = DuplicateClassNamesCDE [ClassDecl t]  -- classes whose name is defined multiple times
    | UndefinedSuperclassCDE [ClassDecl t]  -- classes having undefined superclasses
    | CyclicClassHierarchyCDE [ClassDecl t]    -- classes involved in a cyclic class definition
  deriving (Eq, Ord, Show, Read)

data FieldDeclsError t 
    = DuplicateFieldNamesFDE [(ClassDecl t, [FieldDecl t])]     -- field names with duplicate defs
    | UndefinedTypeFDE [FieldDecl t]              -- field names with duplicate defs
  deriving (Eq, Ord, Show, Read)

data VarDeclsError t
    = DuplicateVarNamesVDE [VarDecl t]
    | UndefinedTypeVDE [VarDecl t]
  deriving (Eq, Ord, Show, Read)

data RuleError t 
  = RuleErrorRE    -- TODO: refine this error type
  deriving (Eq, Ord, Show, Read)

data AssertionError t 
  -- first list: expressions not of type Boolean; 
  -- second list: expressions that are ill-typed
  = AssertionErrAE [Expr t] [ErrorCause]   
  deriving (Eq, Ord, Show, Read)

data Error t 
    = ClassDeclsErr (ClassDeclsError t)
    | FieldDeclsErr (FieldDeclsError t)  
    | VarDeclsErr (VarDeclsError t)
    | RuleErr (RuleError t)
    | AssertionErr (AssertionError t)
  deriving (Eq, Ord, Show, Read)



----------------------------------------------------------------------
-- Printing errors
----------------------------------------------------------------------
printPos :: Pos -> String 
printPos (Pos l c) = "(" ++ show l ++ "," ++ show c ++ ")"

printSRng :: SRng -> String
--printSRng = show
printSRng (SRng spos epos) = (printPos spos) ++ " .. " ++ (printPos epos)

printErrorCause :: ErrorCause -> String
printErrorCause (IllTypedSubExpr rngs givents expts) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines 
  (map (\(r, gv, exp) -> "Subexpression at " ++ (printSRng r) ++ " has type " ++ (show gv) ++ " but " ++ (show exp) ++  " was expected") 
  (zip3 (tail rngs) givents expts))
printErrorCause (IncompatibleTp rngs givents) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines 
  (map (\(r, gv) -> "Subexpression at " ++ (printSRng r) ++ " has type " ++ (show gv)) 
  (zip (tail rngs) givents)) ++
  "The types are not compatible (one is subtype of the other)"
printErrorCause (NonScalarExpr rngs givents) =
  "Expression at " ++ (printSRng (head rngs)) ++ " is ill-typed:\n" ++
  unlines 
  (map (\(r, gv) -> "Subexpression at " ++ (printSRng r) ++ " has type " ++ (show gv)) 
  (zip (tail rngs) givents)) ++
  "At least one type is not scalar (non-functional)"
printErrorCause c = show c 

printAssertionErr :: AssertionError t -> String 
printAssertionErr (AssertionErrAE nonBoolExprs illTyped) =
  unlines (map printErrorCause illTyped)

printError :: Show t => Error t -> String 
printError e = case e of
  AssertionErr ae -> printAssertionErr ae
  unknownErr -> show unknownErr
  
