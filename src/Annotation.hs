
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Annotation where

import Data.Data (Data, Typeable)

----------------------------------------------------------------------
-- Positions and Ranges
----------------------------------------------------------------------

data Pos = Pos
  { line :: !Int
  , col  :: !Int
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

nullPos :: Pos
nullPos = Pos 0 0

data SRng = SRng
  { start :: Pos
  , end   :: Pos
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

nullSRng :: SRng
nullSRng = SRng nullPos nullPos

coordFromTo :: SRng -> SRng -> SRng
coordFromTo (SRng f1 t1) (SRng f2 t2) = SRng (min f1 f2) (max t2 t2)

tokenRange :: (HasLoc f, HasLoc g) => f -> g -> SRng
tokenRange a b = coordFromTo (getLoc a) (getLoc b)

tokenRangeList :: [SRng] -> SRng
tokenRangeList [] = nullSRng
tokenRangeList [x] = getLoc x
tokenRangeList (x:xs) = tokenRange x (tokenRangeList xs)

class HasLoc a where
  getLoc :: a -> SRng

instance HasLoc SRng where
  getLoc = id

instance HasLoc a => HasLoc [a] where
  getLoc = tokenRangeList . map getLoc

data LocTypeAnnot a = LocTypeAnnot { locAnnot :: SRng
                                   , typeAnnot :: a
}
  deriving (Eq, Ord, Show, Read, Data, Typeable)


instance HasLoc (LocTypeAnnot a) where
  getLoc = locAnnot

class TypeAnnot f where
  getType :: f a -> a
  updType :: f a -> b -> f b

instance TypeAnnot LocTypeAnnot where
  getType = typeAnnot
  updType lta v = lta {typeAnnot = v}


