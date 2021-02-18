module ToGF where

import Syntax
import Prop -- the generated Haskell abstract syntax from the GF
import PGF
import Paths_baby_l4


myPgf :: IO PGF
myPgf = do
  pgfFile <- Paths_baby_l4.getDataFileName "Prop.pgf"
  PGF.readPGF pgfFile

printPgf :: IO ()
printPgf = print . categories =<< myPgf

expr2prop :: Syntax.Expr Tp -> GProp
expr2prop e = case e of
  ValE _ val -> GPAtom (val2atom val)
  _ -> undefined


val2atom :: Val -> GAtom
val2atom e  = case e of
--  BoolV b ->
  IntV i -> GAKind GNat (GIInt (GInt (fromInteger i)))
  _ -> undefined
