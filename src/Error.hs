
module Error where
import Syntax

data ClassDeclsError 
    = DuplicateClassNamesCDE              -- TODO: more informative error: class names with duplicate defs
    | UndefinedSuperclassCDE [ClassName]  -- classes having undefined superclasses
    | CyclicClassHierarchy [ClassName]    -- classes involved in a cyclic class definition
  deriving (Eq, Ord, Show, Read)