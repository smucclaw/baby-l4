
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Annotation where

import Data.Data (Data, Typeable)

----------------------------------------------------------------------
-- Positions and Ranges
----------------------------------------------------------------------

data Located a = L
  { loc :: SRng
  , unLoc :: a
  }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance Applicative Located where
  pure = L $ DummySRng "Unknown position"
  L lf f <*> L lx x = L (coordFromTo lf lx) $ f x

instance HasLoc (Located a) where
  getLoc = loc

mkLocated :: HasLoc a => a -> Located a
mkLocated x = L (getLoc x) x

class HasAnnot f where
  getAnnot :: f a -> a
  updateAnnot :: (a -> a) -> f a -> f a

setAnnot :: HasAnnot f => a -> f a -> f a
setAnnot = updateAnnot . const

-- | Annotate an ast node with the position from a wrapping Located
loc2Ann :: HasAnnot f => Located (f SRng) -> f SRng
loc2Ann (L loc p) = setAnnot loc p

data Pos = Pos
  { line :: !Int
  , col  :: !Int
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

nullPos :: Pos
nullPos = Pos 0 0

type Reason = String

data SRng =
  RealSRng RealSRng
  | DummySRng Reason
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data RealSRng = SRng
  { start :: Pos
  , end   :: Pos
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

nullSRng :: SRng
nullSRng = DummySRng "Empty list"

instance Semigroup RealSRng where
  SRng f1 t1 <> SRng f2 t2 = SRng (min f1 f2) (max t2 t2)

instance Semigroup SRng where
  (<>) = coordFromTo

instance Monoid SRng where
  mempty = nullSRng

coordFromTo :: SRng -> SRng -> SRng
coordFromTo (RealSRng r) (RealSRng r1) = RealSRng $ r <> r1
coordFromTo r (DummySRng _) = r
coordFromTo (DummySRng _) r = r

tokenRange :: (HasLoc f, HasLoc g) => f -> g -> SRng
tokenRange a b = getLoc a <> getLoc b

tokenRangeList :: [SRng] -> SRng
tokenRangeList = mconcat

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


