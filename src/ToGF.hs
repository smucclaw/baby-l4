module ToGF where

import Syntax
import Prop -- the generated Haskell abstract syntax from the GF
--import PGF

expr2prop :: Expr Tp -> GProp
expr2prop e = case e of
  ValE _ val -> GPAtom (val2atom val)
  _ -> undefined


val2atom :: Val -> GAtom
val2atom e  = case e of
--  BoolV b ->
  IntV i -> GAKind GNat (GIInt (GInt i))
  _ -> undefined
