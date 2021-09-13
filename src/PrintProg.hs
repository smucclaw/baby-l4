{-# LANGUAGE EmptyCase #-}
module PrintProg where

import Syntax
import KeyValueMap ( KVMap )
import Error (printTp)


-------------------------------------------------------------
-- Rename variables in rules and expressions
-------------------------------------------------------------

-- Rename variables to make them unique in declarations in rules and in expressions.
-- Assumption: Global variables are already unique, correct use of deBruijn indices

mkUniqueName :: [String] -> String -> Int -> String
mkUniqueName nms nm ext =
    if nm `notElem` nms
    then nm
    else
        let sugg = nm ++ "_" ++ show ext
        in if sugg `notElem` nms
           then sugg
           else mkUniqueName nms nm (ext+1)

renameVarDecl :: [String] -> VarDecl t -> (String, VarDecl t)
renameVarDecl nms vd =
    let nnm = mkUniqueName nms (nameOfVarDecl vd) 0
    in (nnm, vd{nameOfVarDecl = nnm})

renameVarDecls :: [String] -> [VarDecl t] -> [VarDecl t] -> ([String], [VarDecl t])
renameVarDecls nms accvds [] = (nms, reverse accvds)
renameVarDecls nms accvds (vd:vds) =
    let (nnm, nvd) = renameVarDecl nms vd
    in renameVarDecls (nnm:nms) (nvd:accvds) vds

renameQVarName :: [String] -> QVarName t -> (String, QVarName t)
renameQVarName nms qvn =
    let nnm = mkUniqueName nms (nameOfQVarName qvn) 0
    in (nnm, qvn{nameOfQVarName=nnm})

renameQVarNames :: [String] -> [QVarName t] -> [QVarName t] -> ([String], [QVarName t])
renameQVarNames nms accqvns [] = (nms, reverse accqvns)
renameQVarNames nms accqvns (qvn:qvns) =
    let (nnm, nqvn) = renameQVarName nms qvn
    in renameQVarNames (nnm:nms) (nqvn:accqvns) qvns

renameVar :: [String] -> Var t -> Var t
renameVar nms v@(GlobalVar _) = v
renameVar nms v@(LocalVar vn i) =
    let nnm = nms!!i
        nqvn = vn{nameOfQVarName = nnm}
    in v{nameOfVar=nqvn}

renamePattern :: [String] -> Pattern t -> ([String], Pattern t)
renamePattern nms (VarP qvn) = let (nnm, nqvn) = renameQVarName nms qvn in (nnm:nms, VarP nqvn)
renamePattern nms (VarListP qvns) = let (nnms, nqvns) = renameQVarNames nms [] qvns in (nnms, VarListP nqvns)

renameExpr :: [String] -> Expr t -> Expr t
renameExpr nms e@ValE {} = e
renameExpr nms e@VarE {varOfExprVarE = v} = e{varOfExprVarE = renameVar nms v}
renameExpr nms e@UnaOpE {subEOfExprUnaOpE = se} = e{subEOfExprUnaOpE = renameExpr nms se}
renameExpr nms e@BinOpE {subE1OfExprBinOpE = se1, subE2OfExprBinOpE = se2} =
        e{subE1OfExprBinOpE = renameExpr nms se1, subE2OfExprBinOpE = renameExpr nms se2}
renameExpr nms (IfThenElseE ann ce te ee) = IfThenElseE ann (renameExpr nms ce) (renameExpr nms te) (renameExpr nms ee)
renameExpr nms (AppE ann f a) = AppE ann (renameExpr nms f) (renameExpr nms a)
renameExpr nms (FunE ann v bd) =
    let (nnm, nvn) = renameVarDecl nms v
    in FunE ann nvn (renameExpr (nnm:nms) bd)
renameExpr nms (QuantifE ann q v bd) =
    let (nnm, nvn) = renameVarDecl nms v
    in QuantifE ann q nvn (renameExpr (nnm:nms) bd)
renameExpr nms e@FldAccE {subEOfExprFldAccE = se} = e{subEOfExprFldAccE = renameExpr nms se}
renameExpr nms (TupleE ann es) = TupleE ann (map (renameExpr nms) es)
renameExpr nms e@CastE {subEOfExprCastE = se} = e{subEOfExprCastE = renameExpr nms se}
renameExpr nms (ListE ann lop es) = ListE ann lop (map (renameExpr nms) es)


-- Rename rules, assuming that the names in the namelist nms are already used
-- In nms, more recent names are added at the beginning 
renameRule :: [String] -> Rule t -> Rule t
renameRule nms rl =
    let (nnms, rnmdVarDecls) = renameVarDecls nms [] (varDeclsOfRule rl)
        rnmdPrecond = renameExpr nnms (precondOfRule rl)
        rnmdPostcond = renameExpr nnms (postcondOfRule rl)
    in rl{varDeclsOfRule = rnmdVarDecls, precondOfRule = rnmdPrecond, postcondOfRule = rnmdPostcond}


-------------------------------------------------------------
-- Print expressions and parts of programs
-------------------------------------------------------------

printARName :: ARName -> String
printARName Nothing = ""
printARName (Just s) = "<" ++ s ++ ">"

printInstr :: KVMap -> String
printInstr = show

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
printVal (FloatV i) = show i
printVal (StringV s) = show s
printVal v = show v    -- TODO - rest still to be done

printVar :: Var t -> String
printVar = nameOfQVarName . nameOfVar
-- For debugging:
--printVar (GlobalVar qvn) = nameOfQVarName qvn
--printVar (LocalVar qvn i) = nameOfQVarName qvn ++ "@" ++ show i

printUTemporalOp :: UTemporalOp -> String 
printUTemporalOp UTAF = "A<>"
printUTemporalOp UTAG = "A[]"
printUTemporalOp UTEF = "E<>"
printUTemporalOp UTEG = "E[]"

printUnaOpE :: UnaOp -> String
printUnaOpE (UArith UAminus) = "-"
printUnaOpE (UBool UBnot) = "not "
printUnaOpE (UTemporal ut) = printUTemporalOp ut

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
        BCeq -> "=="
        BClt -> "<"
        BClte -> "<="
        BCgt -> ">"
        BCgte -> ">="
        BCne -> "/="
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
printExpr (FunE t v et) = "( \\ " ++ printVarDecl v ++ " -> " ++ printExpr et ++ ")"
printExpr (QuantifE t q v et) = "(" ++ printQuantif q ++ printVarDecl v ++ ". " ++ printExpr et ++ ")"
printExpr (FldAccE t et f) = printExpr et ++ "." ++ stringOfFieldName f
printExpr e = show e  -- TODO - incomplete

printRule :: Show t => Rule t -> String
printRule r =
    "rule " ++ printARName (nameOfRule r) ++ "\n" ++
    printInstr (instrOfRule r) ++ "\n" ++
    (let vds = printVarDeclsCommaSep (varDeclsOfRule r) in if vds == "" then "" else vds ++ "\n") ++
    "if   " ++ printExpr (precondOfRule r) ++ "\n" ++
    "then " ++ printExpr (postcondOfRule r) ++ "\n\n"

namesUsedInProgram :: Program t -> [VarName]
namesUsedInProgram = map nameOfVarDecl . globalsOfProgram

renameAndPrintExpr :: Show t => [String] -> Expr t -> String
renameAndPrintExpr nms  = printExpr . renameExpr nms

renameAndPrintRule :: Show t => [String] -> Rule t -> String
renameAndPrintRule nms  = printRule . renameRule nms
