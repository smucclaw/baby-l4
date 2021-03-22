
{-# LANGUAGE DeriveDataTypeable #-}

module Annotation where 

import Data.Data (Data, Typeable)

-- moved here from Syntax.hs
-- Positions and Ranges

data Pos = Pos
  { line :: !Int
  , col  :: !Int
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)
data SRng = SRng
  { start :: Pos
  , end   :: Pos
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)


class HasLoc a where
  getLoc :: a -> SRng

instance HasLoc SRng where
  getLoc = id

-- end move from Syntax.hs

-- TODO: the following class might supersede the HasLoc class
class LocAnnot f where
  getLocOfAnnot :: f SRng -> SRng
  updLoc :: f SRng -> SRng -> f SRng

data LocTypeAnnot a = LocTypeAnnot { locAnnot :: SRng 
                                   , typeAnnot :: a
}
    deriving (Eq, Ord, Show, Read, Data, Typeable)


instance HasLoc (LocTypeAnnot a) where
  getLoc = locAnnot

class TypeAnnot f where
  getType :: f a -> a
  updType :: f a -> b -> f b

instance LocAnnot LocTypeAnnot where
  getLocOfAnnot = locAnnot
  updLoc lta l = lta {locAnnot = l} 

instance TypeAnnot LocTypeAnnot where
  getType = typeAnnot
  updType lta v = lta {typeAnnot = v} 

