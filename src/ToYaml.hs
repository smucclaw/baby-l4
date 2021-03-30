module ToYaml where

import Data.Char (toLower, toUpper)
import Syntax
import ToGF.FromL4.ToProp
import Data.List

class Datablob x where
  mkName, mkAsk, mkEncode :: x -> String
  mkType :: x -> Pythontype

data Pythontype = PString | PEnum

lowerString :: String -> String
lowerString = map toLower

listToName (t, l_et) = intercalate "_" $ map ((mkName t++) . showExpr) l_et

toBool :: Bool -> String
toBool = map toLower . show

instance Datablob Tp where
  mkName tp = case tp of
    ClassT (ClsNm str) -> str
    BoolT -> "bool"
    IntT -> "int"
    _ -> "unsupportedtype"
  mkAsk tp = case tp of
    ClassT (ClsNm str) -> str
    BoolT -> "bool"
    IntT -> "int"
    _ -> "unsupportedtype"
  mkType tp = PString
  mkEncode tp = case tp of
    ClassT (ClsNm str) -> str
    BoolT -> "bool"
    IntT -> "int"
    _ -> "unsupportedtype"

instance Datablob Val where
  mkName = showVal
  mkAsk = showVal
  mkType x = PString
  mkEncode = showVal

showVal :: Val -> String
showVal (BoolV False) = "false"
showVal (BoolV True) = "true"
showVal (IntV i) = show i
showVal (StringV l_c) = l_c
showVal (RecordV (ClsNm l_c) _) = l_c
showVal ErrV = []

instance Datablob Var where
  mkName = showVar
  mkAsk = showVar
  mkType x = PString
  mkEncode = showVar

showVar :: Var -> String
showVar (GlobalVar l_c) = l_c
showVar (LocalVar l_c _) = l_c

instance Datablob (Expr t) where
  mkName = showExpr
  mkAsk = showExpr
  mkType x = PString
  mkEncode = showExpr

showExpr :: (Datablob t) => Expr t -> String
showExpr (ValE s t v) = lowerString $ mkName t ++ mkName v -- player1
showExpr (VarE s t v) = lowerString $ mkName t ++ mkName v
--showExpr (UnaOpE s t u et) = _
showExpr e@(BinOpE s t b et et8) = showExpr $ toList e
showExpr (IfThenElseE s t et et7 et8) = "if_" ++ showExpr et ++ "_then_" ++ showExpr et7 ++ "_else_" ++ showExpr et8
showExpr (FunApp1 f x xTp) = mkName f ++ "_" ++ mkName x ++ "_" ++ mkName xTp
showExpr (FunApp2 f x xTp y yTp) = mkName f ++ "_" ++ mkName x ++ "_" ++ mkName xTp ++ "_" ++ mkName y ++ "_" ++ mkName yTp
showExpr (QuantifE s t q l_c t8 et) = mkName (toBool q) ++ "_" ++ mkName t8 ++ mkName l_c ++ showExpr et
--forall_player1
showExpr (FldAccE s t et f) = mkName t ++ showExpr et ++ "_" ++ mkName f
--player1_field
showExpr (TupleE s t l_et) = listToName (t, l_et)
showExpr (CastE s t t6 et) = mkName t6 ++ showExpr et
-- if cat is new type t6, catplayer1
showExpr (ListE s t l l_et) = listToName (t, l_et)
-- playera_playerb_playerc
showExpr (NotDeriv s t b v et) = mkName (toBool b) ++ "_" ++ mkName v ++ "_" ++ showExpr et
-- not_true_player1