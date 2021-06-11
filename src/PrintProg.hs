{-# LANGUAGE EmptyCase #-}
module PrintProg where

import Syntax
import Error (printTp)

printARName :: ARName -> String 
printARName Nothing = ""
printARName (Just s) = "<" ++ s ++ ">"

printQVarName :: QVarName t -> VarName
printQVarName =  nameOfQVarName

printVarDecl :: Show t => VarDecl t -> String
printVarDecl vd = nameOfVarDecl vd ++ ": " ++ printTp (tpOfVarDecl vd)

printVarDeclsCommaSep :: Show t => [VarDecl t] -> String
-- printVarDeclsCommaSep vds = show vds
printVarDeclsCommaSep [] = ""
printVarDeclsCommaSep vds = "for " ++ foldr1 (\w s -> w ++ ", "++s) (map printVarDecl vds)

printVal :: Val -> String
printVal (BoolV b) = show b
printVal (IntV i) = show i
printVal (StringV s) = show s
printVal v = show v    -- TODO - rest still to be done

printVar :: Var t -> String
printVar = nameOfQVarName . nameOfVar

printUnaOpE :: UnaOp -> String
printUnaOpE (UArith UAminus) = "-"
printUnaOpE (UBool UBnot) = "not "

printBinOpE :: BinOp -> String
printBinOpE (BArith b1) =
    case b1 of
        BAadd -> "+"
        BAsub -> "-"
        BAmul -> "*"
        BAdiv -> "/"
        BAmod -> "%"
printBinOpE (BCompar b1) =
    case b1 of
        BCeq -> "="
        BClt -> "<"
        BClte -> "<="
        BCgt -> ">"
        BCgte -> ">="
        BCne -> "!="
printBinOpE (BBool b1) =
    case b1 of
        BBimpl -> "-->"
        BBor -> "||"
        BBand -> "&&"

printPattern :: Pattern t -> String
printPattern (VarP s) = printQVarName s
printPattern (VarListP ss) = "(" ++ foldr1 (\w s -> w ++ ", "++ s ) (map printQVarName ss) ++ ")"

printQuantif :: Quantif -> String
printQuantif All = " all "
printQuantif Ex = " exists "

printExpr :: Show t => Expr t -> String
printExpr (ValE t v) = printVal v
printExpr (VarE t v) = printVar v
printExpr (UnaOpE t u et) = "(" ++ printUnaOpE u ++ printExpr et ++ ")"
printExpr (BinOpE t b et1 et2) = "(" ++ printExpr et1 ++ printBinOpE b ++ printExpr et2 ++ ")"
printExpr (IfThenElseE t c et1 et2) = " if " ++ printExpr c ++ " then " ++ printExpr et1 ++ " else " ++ printExpr et2
printExpr (AppE t f a) = "(" ++ printExpr f ++ " " ++ printExpr a ++ ")"
printExpr (FunE t p pt et) = "( \\ " ++ printPattern p ++ ": " ++ printTp pt ++ " " ++ printExpr et ++ ")"
printExpr (QuantifE t q vn vt et) = "(" ++ printQuantif q ++ printQVarName vn ++ ": " ++ printTp vt ++ ". " ++ printExpr et ++ ")"
printExpr (FldAccE t et f) = printExpr et ++ "." ++ stringOfFieldName f
printExpr e = show e  -- TODO - incomplete

printRule :: Show t => Rule t -> String
printRule r =
    "rule " ++ printARName (nameOfRule r) ++ "\n" ++
    printVarDeclsCommaSep (varDeclsOfRule r) ++ "\n" ++
    "if   " ++ printExpr (precondOfRule r) ++ "\n" ++
    "then " ++ printExpr (postcondOfRule r) ++ "\n"



