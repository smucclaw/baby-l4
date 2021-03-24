
module Error where

data ClassDeclsError 
    = OkCDE
    | DuplicateClassNamesCDE 
    | UndefinedSuperclassCDE
  deriving (Eq, Ord, Show, Read)