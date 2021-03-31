
module Error where
import Syntax

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
  -- first list: expressions not of type; 
  -- second list: expressions that are ill-typed
  = AssertionErrAE [Expr t] [Expr t]   
  deriving (Eq, Ord, Show, Read)

data Error t 
    = ClassDeclsErr (ClassDeclsError t)
    | FieldDeclsErr (FieldDeclsError t)  
    | VarDeclsErr (VarDeclsError t)
    | RuleErr (RuleError t)
    | AssertionErr (AssertionError t)
  deriving (Eq, Ord, Show, Read)