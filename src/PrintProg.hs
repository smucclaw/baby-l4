{-# LANGUAGE LambdaCase #-}


module PrintProg where

import L4.Syntax
import L4.KeyValueMap ( KVMap )
import L4.Prettyprinter
--import Prettyprinter.Render.Text (putDoc)
--import qualified Data.Maybe
import Data.List (find)
--import Data.Maybe (fromMaybe)
import Util (capitalise)
import SyntaxManipulation (appToFunArgs)

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
renameVar _nms v@(GlobalVar _) = v
renameVar nms v@(LocalVar vn i) =
    let nnm = nms!!i
        nqvn = vn{nameOfQVarName = nnm}
    in v{nameOfVar=nqvn}

renamePattern :: [String] -> Pattern t -> ([String], Pattern t)
renamePattern nms (VarP qvn) = let (nnm, nqvn) = renameQVarName nms qvn in (nnm:nms, VarP nqvn)
renamePattern nms (VarListP qvns) = let (nnms, nqvns) = renameQVarNames nms [] qvns in (nnms, VarListP nqvns)

renameExpr :: [String] -> Expr t -> Expr t
renameExpr _nms e@ValE {} = e
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

printTp :: Tp t -> String
printTp tp = case tp of
  ClassT _ cn -> stringOfClassName cn
  FunT _ t1 t2 -> "(" ++ printTp t1 ++ " -> " ++ printTp t2 ++")"
  TupleT _ [] -> "()"
  TupleT _ [t] -> "(" ++ printTp t ++ ")"
  TupleT _ (t:ts) -> "(" ++ printTp t ++ ", " ++ foldr (\s r -> (printTp s ++ ", " ++ r)) "" ts ++ ")"
  _ -> error "internal error in printTp: ErrT or OkT not printable"

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


-------------------------------------------------------------
-- Print expressions etc. using Prettyprinter library
-------------------------------------------------------------

-- Configuration of printing functions. 
-- The first constructor of the following types should be the default
data PrintVarIndex
    = PrintName                             -- print local variable name without additional info
    | PrintIndexedName                      -- print local variable name with de Bruijn index (for debugging)
data PrintVarCase
    = AsGiven                               -- print names as given
    | CapitalizeLocalVar                    -- capitalize (first letter of) local variables
    | CapitalizeVar                         -- capitalize all variables

data PrintCurried
    = Curried                               -- print function application curried: f a1 a2 .. an
    | MultiArg                              -- print application as function + list of args: f(a1, .., an)

data PrintConfig =
      PrintVarIndex PrintVarIndex
    | PrintVarCase PrintVarCase
    | PrintCurried PrintCurried

printVarIndex :: [PrintConfig] -> PrintVarIndex
printVarIndex cfs = case find (\ case PrintVarIndex {} -> True; _ -> False) cfs of
    Just (PrintVarIndex x) -> x
    _ -> PrintName

printVarCase :: [PrintConfig] -> PrintVarCase
printVarCase cfs = case find (\ case PrintVarCase {} -> True; _ -> False) cfs of
    Just (PrintVarCase x) -> x
    _ -> AsGiven

printCurried :: [PrintConfig] -> PrintCurried
printCurried cfs = case find (\ case PrintCurried {} -> True; _ -> False) cfs of
    Just (PrintCurried x) -> x
    _ -> Curried
class ShowL4 x where
    showL4 :: [PrintConfig] -> x -> Doc ann

instance ShowL4 (Tp t) where
  showL4 cfs (ClassT _ cn) = pretty (stringOfClassName cn)
  showL4 cfs (FunT _ t1 t2) = parens (showL4 cfs t1 <+> pretty "->" <+> showL4 cfs t2)
  showL4 cfs (TupleT _ []) = parens (pretty "")
  showL4 cfs (TupleT _ [t]) = parens ( showL4 cfs t )
  -- TODO: see how the following is done
  -- showL4 cfs (TupleT _ (t:ts)) = parens ( showL4 cfs t <+> pretty "," <+> (foldr (\s r -> ((printTp s) ++ ", " ++ r)) "" ts))
  showL4 cfs _ = error "internal error in printTp: ErrT or OkT not printable"

instance ShowL4 Val where
    showL4 _ (BoolV b) = pretty (show b)
    showL4 _ (IntV i) = pretty (show i)
    showL4 _ (FloatV i) = pretty (show i)
    showL4 _ (StringV s) = pretty (show s)
    showL4 _ v = pretty (show v)    -- TODO - rest still to be done

instance ShowL4 (Var t) where
    showL4 cfs (GlobalVar qvn) = pretty name
        where
            name = case printVarCase cfs of
                    CapitalizeVar -> capitalise (nameOfQVarName qvn)
                    _ -> nameOfQVarName qvn
    showL4 cfs (LocalVar qvn i) = pretty name <> pretty index
        where
            name = case printVarCase cfs of
                    AsGiven -> nameOfQVarName qvn
                    _ -> capitalise (nameOfQVarName qvn)
            index = case printVarIndex cfs of
                    PrintName -> ""
                    PrintIndexedName -> "@" ++ show i


instance ShowL4 (VarDecl t) where
    showL4 cfs vd = pretty (nameOfVarDecl vd) <+> pretty ":" <+> showL4 cfs (tpOfVarDecl vd)

instance Show t => ShowL4 (Expr t) where
    showL4 cfs (ValE t v) = showL4 cfs v
    showL4 cfs (VarE t v) = showL4 cfs v
    showL4 cfs (UnaOpE t u et) = parens (pretty (printUnaOpE u) <+> showL4 cfs et )
    showL4 cfs (BinOpE t b et1 et2) = parens (showL4 cfs et1 <+> pretty (printBinOpE b) <+> showL4 cfs et2)
    showL4 cfs (IfThenElseE t c et1 et2) = pretty "if " <+> showL4 cfs c <+> pretty "then" <+> showL4 cfs et1 <+> pretty "else" <+> showL4 cfs et2
    showL4 cfs appe@(AppE t f a) =
        case printCurried cfs of
            Curried -> parens (showL4 cfs f <+> showL4 cfs a)
            MultiArg ->
                let (fct, args) = appToFunArgs [] appe
                in showL4 cfs fct <> parens (hsep (punctuate comma (map (showL4 cfs) args)))
    showL4 cfs (FunE t v et) = parens ( pretty "\\ " <+> showL4 cfs v <+> pretty "->" <+> showL4 cfs et)
    showL4 cfs (QuantifE t q v et) = parens (pretty (printQuantif q) <+> showL4 cfs v <+> pretty "." <+> showL4 cfs et)
    showL4 cfs (FldAccE t et f) = showL4 cfs et <+> pretty "." <+> pretty (stringOfFieldName f)
    showL4 cfs e = pretty (show e)  -- TODO - incomplete

instance Show t => ShowL4 (Rule t) where
    showL4 cfs r =
        vsep [ pretty "rule" <+> pretty (printARName (nameOfRule r))
             -- , printInstr (instrOfRule r) ++ "\n" ++
            -- (let vds = printVarDeclsCommaSep (varDeclsOfRule r) in if vds == "" then "" else vds ++ "\n") ++
             , pretty "if" <+> showL4 cfs (precondOfRule r)
             , pretty "then" <+> showL4 cfs (postcondOfRule r)
             ]
