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

nlg :: (Show ct, Show et) => Program ct et -> IO ()
nlg prog = do
  gr <- myPgf
  sequence_ [
    mapM_ putStrLn $ linearizeAll gr pgfExpr
    | prop <- program2prop prog
    , let pgfExpr = gf prop
    ]

-- Example: Program [] [VarDecl "myNumber" IntT,VarDecl "myTruth" BoolT] [] []
program2prop :: (Show ct, Show et) => Program ct et -> [GProp]
program2prop e = case e of
  Program _cl vardecls _rs _as -> map vardecl2prop vardecls
  _ -> error $ "program2prop: not yet supported: " ++ show e

vardecl2prop :: VarDecl -> GProp
vardecl2prop (VarDecl vname vtyp) = 
  GPAtom (GAKind (typ2kind vtyp) (var2ind vname))

var2ind :: VarName -> GInd
var2ind str = (GIVar (GVString (GString str)))

typ2kind :: Tp -> GKind
typ2kind e = case e of
  BoolT -> GBoolean
  IntT  -> GNat
  _ -> error $ "typ2kind: not yet supported: " ++ show e
  -- ClassT ClassName
  -- FunT Tp Tp
  -- TupleT [Tp]
  -- ErrT

expr2prop :: Syntax.Expr Tp -> GProp
expr2prop e = case e of
  ValE _ val -> GPAtom (val2atom val)
  _ -> error $ "expr2prop: not yet supported: " ++ show e

val2atom :: Val -> GAtom
val2atom e  = case e of
  BoolV True -> GAKind GBoolean GBTrue
  BoolV False -> GAKind GBoolean GBFalse
  IntV i -> GAKind GNat (GIInt (GInt (fromInteger i)))
  _ -> error $ "val2atom: not yet supported: " ++ show e
