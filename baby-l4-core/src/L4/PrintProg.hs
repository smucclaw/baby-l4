{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}


module L4.PrintProg where

import L4.Syntax
import L4.KeyValueMap ( KVMap )
import Prettyprinter
--import Prettyprinter.Render.Text (putDoc)
--import qualified Data.Maybe
import Data.List (find)
--import Data.Maybe (fromMaybe)
--import Util (capitalise)
import L4.SyntaxManipulation (appToFunArgs)

import Data.Char        ( toUpper )
import System.Posix.Types (CDev(CDev))
import L4.Syntax (Program(Program), ClConstr (ClConstr), Transition (Transition), FieldDecl (FieldDecl))
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Prettyprinter.Internal.Type (Doc(Empty))


-------------------------------------------------------------
-- Rename variables in rules and expressions
-------------------------------------------------------------

-- Rename variables to make them unique in declarations in rules and in expressions.
-- Assumption: Global variables are already unique, correct use of deBruijn indices
capitalise :: String -> String
capitalise [] = []
capitalise (c:cs) = toUpper c : cs


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

printBArith :: BArithOp -> String
printBArith b = case b of
        BAadd -> "+"
        BAsub -> "-"
        BAmul -> "*"
        BAdiv -> "/"
        BAmod -> "%"

printBCompar :: BComparOp -> String
printBCompar b = case b of
        BCeq -> "=="
        BClt -> "<"
        BClte -> "<="
        BCgt -> ">"
        BCgte -> ">="
        BCne -> "/="

printBBool :: BBoolOp -> String
printBBool b = case b of
        BBimpl -> "-->"
        BBor -> "||"
        BBand -> "&&"

printBinOpE :: BinOp -> String
printBinOpE (BArith b) = printBArith b
printBinOpE (BCompar b) = printBCompar b
printBinOpE (BBool b) = printBBool b

printPattern :: Pattern t -> String
printPattern (VarP s) = printQVarName s
printPattern (VarListP ss) = "(" ++ foldr1 (\w s -> w ++ ", "++ s ) (map printQVarName ss) ++ ")"

printQuantif :: Quantif -> String
printQuantif All = " all "
printQuantif Ex = " exists "

printExpr :: Show t => Expr t -> String
printExpr (ValE _t v) = printVal v
printExpr (VarE _t v) = printVar v
printExpr (UnaOpE _t u et) = "(" ++ printUnaOpE u ++ printExpr et ++ ")"
printExpr (BinOpE _t b et1 et2) = "(" ++ printExpr et1 ++ printBinOpE b ++ printExpr et2 ++ ")"
printExpr (IfThenElseE _t c et1 et2) = " if " ++ printExpr c ++ " then " ++ printExpr et1 ++ " else " ++ printExpr et2
printExpr (AppE _t f a) = "(" ++ printExpr f ++ " " ++ printExpr a ++ ")"
printExpr (FunE _t v et) = "( \\ " ++ printVarDecl v ++ " -> " ++ printExpr et ++ ")"
printExpr (QuantifE _t q v et) = "(" ++ printQuantif q ++ printVarDecl v ++ ". " ++ printExpr et ++ ")"
printExpr (FldAccE _t et f) = printExpr et ++ "." ++ stringOfFieldName f
printExpr e = show e  -- TODO - incomplete

printRule :: Show t => Rule t -> String
printRule r =
    "rule " ++ printARName (nameOfRule r) ++ "\n" ++
    printInstr (instrOfRule r) ++ "\n" ++
    (let vds = printVarDeclsCommaSep (varDeclsOfRule r) in if vds == "" then "" else vds ++ "\n") ++
    "if   " ++ printExpr (precondOfRule r) ++ "\n" ++
    "then " ++ printExpr (postcondOfRule r) ++ "\n\n"

namesUsedInProgram :: Program t -> [VarName]
namesUsedInProgram = map nameOfVarDecl . varDeclsOfProgram

renameAndPrintExpr :: Show t => [String] -> Expr t -> String
renameAndPrintExpr nms  = printExpr . renameExpr nms

renameAndPrintRule :: Show t => [String] -> Rule t -> String
renameAndPrintRule nms  = printRule . renameRule nms


-------------------------------------------------------------
-- Print expressions etc. using Prettyprinter library
-------------------------------------------------------------


-- ...........................................................
-- Settings and convenience functions
-- indentation depth for substructures
nestingDepth :: Int
nestingDepth = 4

isEmpty :: Doc ann -> Bool
isEmpty Empty = True
isEmpty _ = False

vsepnonempty :: [Doc ann] -> Doc ann
vsepnonempty ds = vsep [d | d <- ds, not (isEmpty d)]

-- depending on whether nobraces is true or not, display as
-- prefix   (without anything else)  or
-- prefix { lst }  (with the part in braces indented)
optionalBraces :: Doc ann -> Bool -> [Doc ann] -> Doc ann
optionalBraces prefix nobraces lst  =
    if nobraces
    then prefix
    else nest nestingDepth
            (vsep ( [prefix <+> pretty "{"] ++
                    lst ++
                    [pretty "}"]))

-- ...........................................................
-- Configuration 
-- The first argument of the printing functions (cfs) configures the printing behaviour,
-- according to the following settings.
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

data PrintSystem
    = L4Style              -- print TASys in L4 style, enclosed in system {...} and composed of the processes it contains
    | UppaalStyle          -- print TASys in a style that can directly be read in by Uppaal (*.xta format)

data PrintConfig =
      PrintVarIndex PrintVarIndex
    | PrintVarCase PrintVarCase
    | PrintCurried PrintCurried
    | PrintSystem PrintSystem

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

printSystem :: [PrintConfig] -> PrintSystem
printSystem cfs = case find (\ case PrintSystem {} -> True; _ -> False) cfs of
    Just (PrintSystem x) -> x
    _ -> L4Style


class ShowL4 x where
    showL4 :: [PrintConfig] -> x -> Doc ann

instance ShowL4 (Tp t) where
  showL4 _cfs (ClassT _ cn) = pretty (stringOfClassName cn)
  showL4 cfs (FunT _ t1 t2) = parens (showL4 cfs t1 <+> pretty "->" <+> showL4 cfs t2)
  showL4 _cfs (TupleT _ []) = parens (pretty "")
  showL4 cfs (TupleT _ [t]) = parens ( showL4 cfs t )
  -- TODO: see how the following is done
  -- showL4 cfs (TupleT _ (t:ts)) = parens ( showL4 cfs t <+> pretty "," <+> (foldr (\s r -> ((printTp s) ++ ", " ++ r)) "" ts))
  showL4 _cfs _ = error "internal error in printTp: ErrT or OkT not printable"

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
instance Show t => ShowL4 (VarDefn t) where
    showL4 cfs vd = pretty (nameOfVarDefn vd) <+> pretty ":" <+> showL4 cfs (tpOfVarDefn vd)
                    <+> pretty "=" <+> showL4 cfs (bodyOfVarDefn vd)
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

instance ShowL4 ClassName where
    showL4 cfs (ClsNm nm) = pretty nm

instance ShowL4 FieldName where
    showL4 cfs (FldNm nm) = pretty nm

-- note: assumption: only strict subclasses of Class are displayed
-- otherwise, head (tail cls) will fail
showExtends :: [PrintConfig] -> [ClassName] -> Doc ann
showExtends cfs cls =
    if ClassC /= head (tail cls)
    then pretty "extends" <+> showL4 cfs (head (tail cls))
    else emptyDoc

showFieldDecls :: Show t => [PrintConfig] -> [FieldDecl t] -> Doc ann
showFieldDecls cfs fds =
    case fds of
        [] -> emptyDoc
        _ -> nest nestingDepth
                (vsep ([pretty "{"] ++
                       map (\fd -> showL4 cfs (nameOfFieldDecl fd) <+> pretty ":" <+> showL4 cfs (tpOfFieldDecl fd) ) fds ++
                       [pretty "}"]))


-- note: only strict subclasses of Class are displayed
instance Show t => ShowL4 (ClassDecl t) where
    showL4 cfs cd =
        if ClassC `elem` tail (supersOfClassDef (defOfClassDecl cd))
        then optionalBraces (hsep [pretty "class"
                                  , showL4 cfs (nameOfClassDecl cd)
                                  , showExtends cfs (supersOfClassDef (defOfClassDecl cd ))])
                            (null (fieldsOfClassDef (defOfClassDecl cd )))
                            (map (\fd -> showL4 cfs (nameOfFieldDecl fd) <+> pretty ":" <+> showL4 cfs (tpOfFieldDecl fd) ) (fieldsOfClassDef (defOfClassDecl cd )))
        else emptyDoc


instance ShowL4 Clock where
    showL4 _cfs (Clock nm) = pretty nm

instance ShowL4 ClConstr where
    showL4 cfs (ClConstr cl cop i) =
        showL4 cfs cl <+> pretty (printBCompar cop) <+> pretty i
instance ShowL4 Loc where
    showL4 _cfs (Loc nm) = pretty nm

instance ShowL4 SyncMode where
    showL4 _cfs Broadcast = emptyDoc
    showL4 _cfs Send = pretty "!"
    showL4 _cfs Receive = pretty "?"

instance Show t => ShowL4 (TransitionGuard t) where
    showL4 cfs (TransitionGuard [] Nothing) = emptyDoc
    showL4 cfs (TransitionGuard constrts Nothing) =
        hsep ([pretty "guard"] ++ showInvars cfs constrts ++ [pretty ";"])
    showL4 cfs (TransitionGuard constrts (Just e)) =
        hsep ([pretty "guard"] ++ showInvars cfs constrts ++ [pretty " && ", showL4 cfs e, pretty ";"])

instance Show t => ShowL4 (TransitionAction t) where
    showL4 cfs (TransitionAction [] _act) = emptyDoc
    showL4 cfs (TransitionAction clocks _act) =
        hsep (pretty "assign" : punctuate comma (map (\c -> showL4 cfs c <+> pretty "= 0") clocks)) <> pretty ";"
instance Show t => ShowL4 (Transition t) where
    showL4 cfs t =
        hsep [ showL4 cfs (sourceOfTransition t)
             , pretty "->"
             , showL4 cfs (targetOfTransition t)
             , pretty "{"
             , showL4 cfs (guardOfTransition t)
             , showSync cfs (syncOfTransition t)
             , showL4 cfs (actionOfTransition t)
             , pretty "}"
        ]

showSync :: [PrintConfig] -> Maybe Sync -> Doc ann
showSync _cfs Nothing = emptyDoc
showSync cfs (Just (Sync nm md)) = pretty "sync" <+> pretty nm <> showL4 cfs md <> pretty ";"


showInvars :: [PrintConfig] -> [ClConstr] -> [Doc ann]
showInvars cfs constrts = punctuate (pretty " &&") (map (showL4 cfs) constrts)

showConstraints :: [PrintConfig] -> Maybe [ClConstr] -> Doc ann
showConstraints _cfs Nothing = emptyDoc
showConstraints _cfs (Just []) = emptyDoc
showConstraints cfs (Just constrts) =
    hsep ( [ pretty " {" ] ++
           showInvars cfs constrts ++
           [ pretty "}" ]
    )


showState :: [PrintConfig] -> [Loc] -> [(Loc, [ClConstr])] -> Doc ann
showState cfs locs constrts =
    nest nestingDepth
        (vsep ([ pretty "state" ] ++
                punctuate comma (map (\l -> pretty (nameOfLoc l) <> showConstraints cfs (lookup l constrts)) locs) ++
                [ pretty ";"])
        )

showTrans :: Show t => [PrintConfig] -> [Transition t] -> Doc ann
showTrans cfs trans =
    nest nestingDepth
        (vsep ([ pretty "trans" ] ++
               punctuate comma (map (showL4 cfs) trans) ++
               [ pretty ";"])
        )

showClockDecls :: [PrintConfig] -> [Clock] -> Doc ann
showClockDecls _cfs [] = emptyDoc 
showClockDecls cfs clocks =  pretty "clock" <+> hsep (punctuate comma (map (showL4 cfs) clocks)) <> pretty ";"
instance Show t => ShowL4 (TA t) where
    showL4 cfs aut =
        optionalBraces (pretty "process" <+> pretty (nameOfTA aut) <+> pretty "()") False 
             [ showClockDecls cfs (clocksOfTA aut)
             , showState cfs (locsOfTA aut) (invarsOfTA aut)
             , pretty "init" <+> showL4 cfs (initialLocOfTA aut) <> pretty ";"
             , showTrans cfs (transitionsOfTA aut)]


printL4System :: Show t => [PrintConfig] -> TASys t -> Doc ann
printL4System cfs sys =
    optionalBraces (pretty "system" <+> pretty (nameOfTASys sys)) False 
          ((pretty "chan" <+> hsep (punctuate comma (map pretty (channelsOfSys sys))) <> pretty ";") :
           map (showL4 cfs) (automataOfSys sys))

printUppaalSystem :: Show t => [PrintConfig] -> TASys t -> Doc ann
printUppaalSystem cfs sys =
    vsep ([ pretty "chan" <+> hsep (punctuate comma (map pretty (channelsOfSys sys))) <> pretty ";" ] ++
          map (showL4 cfs) (automataOfSys sys) ++
          [ pretty "system" <+> hsep (punctuate comma (map (pretty . nameOfTA) (automataOfSys sys))) <> pretty ";" ])
instance Show t => ShowL4 (TASys t) where
    showL4 cfs sys =
        case printSystem cfs of
            L4Style -> printL4System cfs sys
            UppaalStyle -> printUppaalSystem cfs sys

instance Show t => ShowL4 (Rule t) where
    showL4 cfs r =
        vsep ([ pretty "rule" <+> pretty (printARName (nameOfRule r)) ] ++
             -- , printInstr (instrOfRule r) ++ "\n" ++
             [hsep (punctuate comma (map (showL4 cfs) (varDeclsOfRule r)))] ++
             [ pretty "if" <+> showL4 cfs (precondOfRule r)
             , pretty "then" <+> showL4 cfs (postcondOfRule r)
             ])

instance Show t => ShowL4 (Assertion t) where
    showL4 cfs a =
        vsep [ pretty "assert" <+> pretty (printARName (nameOfAssertion a))
             -- , printInstr (instrOfAssertion a) ++ "\n" ++
             , showL4 cfs (exprOfAssertion a)
             ]

instance Show t => ShowL4 (TopLevelElement t) where
    showL4 _cfs (MappingTLE m) = pretty "Mapping: ShowL4 not implemented"
    showL4 cfs (ClassDeclTLE cd) = showL4 cfs cd
    showL4 cfs (VarDeclTLE vd) = pretty "decl" <+> showL4 cfs vd
    showL4 cfs (VarDefnTLE vd) = pretty "defn" <+> showL4 cfs vd
    showL4 cfs (RuleTLE r) = showL4 cfs r
    showL4 cfs (AssertionTLE a) = showL4 cfs a
    showL4 cfs (AutomatonTLE a) = showL4 cfs a
    showL4 cfs (SystemTLE s) = showL4 cfs s

instance Show t => ShowL4 (Program t) where
    showL4 cfs prg = vsepnonempty (map (showL4 cfs) (elementsOfProgram prg))

printTest :: Show t => Program t -> PrintSystem -> IO()
printTest p style = print (showL4 [PrintSystem style] p)
