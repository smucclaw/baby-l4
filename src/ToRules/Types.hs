{-# LANGUAGE NamedFieldPuns #-}
module ToRules.Types where

import Prettyprinter
import L4.Syntax
import Util (capitalise)


-- Some notes:
-- Given that CE & FE expressions share such similar structure, it might be good for us to 
-- define some lower-level types that can generalize the similariites, and use CE and FE as wrappers

data RuleFormat = Clara | Drools deriving Eq

showForm :: (ShowDrools x, ShowClara x) => RuleFormat -> (x -> Doc ann)
showForm rf = case rf of
            Drools -> showDrools
            Clara -> showClara

class ShowClara x where
    showClara :: x -> Doc ann

class ShowDrools x where
    showDrools :: x -> Doc ann
----------------------------------------------------------------------------------------------------------------
-- Types & Instances
----------------------------------------------------------------------------------------------------------------

type Typename = String
type ProdVarName = String
type ProdFieldName = String
type ProdFuncName = String
type ProdAnnot = String

type RCList = [ConditionalElement]
type RAList = [RuleAction]

-- anythin marked string is non-fleshed
data ProductionSystem = ProductionSystem { package :: String
                                         , imports :: String
                                         , functions :: [ProductionDefn] 
                                         , queries :: String
                                         , classDecls :: [ProductionClassDecl]
                                         , globals :: String
                                         , rules :: [ProductionRule]
                                         }

data ProductionDefn = ProductionDefn { nameOfProductionDefn :: ProdFuncName 
                                     , returnTpOfProductionDefn :: Typename
                                     , argsOfProductionDefn :: [(ProdVarName, Typename)]
                                     , bodyOfProductionDefn :: ProdFuncExpr }
                                     deriving (Eq, Show)

instance ShowDrools ProductionDefn where
    showDrools ProductionDefn {nameOfProductionDefn, argsOfProductionDefn, returnTpOfProductionDefn, bodyOfProductionDefn} = 
        vsep [ defnHeader <+> lbrace 
             , indent 2 $ pretty "return" <+> (showDrools bodyOfProductionDefn) <> semi
             , rbrace]
        where defnHeader = pretty "function" <+> pretty (capitalise returnTpOfProductionDefn) <+> pretty nameOfProductionDefn <> parens (hsep $ punctuate comma $ map prettyTup argsOfProductionDefn)
              prettyTup (x,y) = pretty x <+> pretty y

instance ShowClara ProductionDefn where
    showClara ProductionDefn {nameOfProductionDefn, argsOfProductionDefn, bodyOfProductionDefn} = 
        parens (vsep [ pretty "defn" <+> pretty nameOfProductionDefn <+> brackets (hsep $ map (pretty . snd) argsOfProductionDefn)
                     , indent 2 $ showClara bodyOfProductionDefn ]) 


data ProdFuncExpr = FEFuncApp ProdFuncName [ProdFuncExpr]
                  | FEComparison BComparOp ProdFuncExpr ProdFuncExpr
                  | FEArithmetic BArithOp ProdFuncExpr ProdFuncExpr
                  | FEVarExpr Argument
                  | FELiteral Val
                  | ProdFuncExprFail String
                  deriving (Eq, Show)

instance ShowDrools ProdFuncExpr where
    showDrools (FEFuncApp nm args) = parens (pretty nm <> parens (hsep $ punctuate comma $ map showDrools args))
    showDrools (FEComparison cOp a1 a2) = parens (showDrools a1 <+> showDrools cOp <+> showDrools a2)
    showDrools (FEArithmetic aOp a1 a2) = parens (showDrools a1 <+> showDrools aOp <+> showDrools a2)
    showDrools (FEVarExpr arg) = showDrools arg
    showDrools (FELiteral val) = showDrools val
    showDrools (ProdFuncExprFail msg) = pretty $ "Error: " ++ msg

instance ShowClara ProdFuncExpr where
    showClara (FEFuncApp nm args) = parens (pretty nm <> parens (hsep $ map showClara args))
    showClara (FEComparison cOp a1 a2) = parens (showClara cOp <+> showClara a1 <+> showClara a2)
    showClara (FEArithmetic aOp a1 a2) = parens (showClara aOp <+> showClara a1 <+> showClara a2)
    showClara (FEVarExpr arg) = showClara arg
    showClara (FELiteral val) = showClara val
    showClara (ProdFuncExprFail msg) = pretty $ "Error: " ++ msg

data ProductionClassDecl = ProductionClassDecl { nameOfProductionClassDecl :: Typename
                                               , fieldsOfProductionClassDecl :: [ProductionClassField]
                                               } deriving Show

instance ShowClara ProductionClassDecl where
    showClara ProductionClassDecl {nameOfProductionClassDecl, fieldsOfProductionClassDecl} = do
        parens (pretty "defrecord" <+> pretty (capitalise nameOfProductionClassDecl) <+> brackets (hsep $ map genField numFields))
        where numFields = [1..length fieldsOfProductionClassDecl]
              genField x = pretty (concat (replicate x "*"))

instance ShowDrools ProductionClassDecl where
    showDrools ProductionClassDecl {nameOfProductionClassDecl, fieldsOfProductionClassDecl} = do
        vsep [ pretty "declare" <+> pretty (capitalise nameOfProductionClassDecl)
             , indent 2 $ vsep (map showDrools fieldsOfProductionClassDecl)
             , pretty "end"
             ]

data ProductionClassField = ProductionClassField ProdFieldName Typename PosAnnot deriving Show
type PosAnnot = String -- positional annotation
instance ShowDrools ProductionClassField where
    showDrools (ProductionClassField pfname tpname pos) = pretty pfname <> colon <+> pretty (yieldTp tpname) <+> pretty "@position" <> parens (pretty pos)



data ProductionRule =
    ProductionRule { nameOfProductionRule :: String
                   , varDeclsOfProductionRule :: [ProdVarName]
                   , leftHandSide :: RCList -- RuleCondition
                   , rightHandSide :: RAList
                   , traceObj :: ProductionRuleTrace
                   } deriving Show

instance ShowClara ProductionRule where
    showClara ProductionRule {nameOfProductionRule, leftHandSide, rightHandSide} = do
        hang 2 $ vsep [ lparen <> pretty "defrule" <+> pretty nameOfProductionRule
                      , vsep (map showClara leftHandSide)
                    --   , showClara leftHandSide
                      , pretty "=>"
                      , vsep (map showClara rightHandSide) <> rparen <> line -- stylisticly, lisps have no dangling brackets
                      ]

instance ShowDrools ProductionRule where
    showDrools ProductionRule {nameOfProductionRule, leftHandSide, rightHandSide, traceObj} = do
        vsep [ nest 2 $ vsep [pretty "rule" <+> dquotes (pretty nameOfProductionRule)
                             , pretty "when"
                            --  , indent 2 $ showDrools leftHandSide
                             , indent 2 $ vsep (map showDrools leftHandSide)
                             , pretty "then"
                             , indent 2 $ vsep (map showDrools rightHandSide)
                             ]
             , pretty "end" <> line
             , showDrools traceObj <> line
             ]

data ProductionRuleTrace = ProductionRuleTrace {
      nameOfTraceObj :: String
    , priors :: [TraceTuple]
    , args :: [TraceTuple]
} deriving (Eq, Show)

newtype TraceTuple = TraceTup (ProdFieldName, Typename) deriving (Eq, Show)

instance ShowClara ProductionRuleTrace where
    showClara x = viaShow x
instance ShowDrools ProductionRuleTrace where
    showDrools ProductionRuleTrace {nameOfTraceObj, priors, args} = 
        showDrools ProductionClassDecl { nameOfProductionClassDecl = nameOfTraceObj ++ " extends Justification"
                                       , fieldsOfProductionClassDecl = traceTupsToProdClassFields 0 (priorsAndArgs 0 0 $ priors <> args)}
        where priorsAndArgs :: Integer -> Integer -> [TraceTuple] -> [(String, Typename)]
              priorsAndArgs pAcc aAcc (TraceTup x:xs) = case x of 
                (_, "Justification") -> ("prior" ++ show pAcc, "Justification") : priorsAndArgs (pAcc + 1) aAcc xs
                (_, y) -> ("arg" ++ show aAcc, y) : priorsAndArgs pAcc (aAcc + 1) xs
              priorsAndArgs _ _ [] = []
              traceTupsToProdClassFields :: Integer -> [(String, Typename)] -> [ProductionClassField]
              traceTupsToProdClassFields cnt ((x,y):tps)  = ProductionClassField x y (show cnt) : traceTupsToProdClassFields (cnt+1) tps
              traceTupsToProdClassFields _ [] = []









-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement
    = ConditionalFuncApp ProdFuncName [CEArg]
    | ConditionalEval BComparOp CEArg CEArg
    | ConditionalExist UBoolOp ConditionalElement -- TODO: rename to Negated Literal
    | ConditionalElementFail String
    deriving (Eq, Show)

instance ShowClara ConditionalElement where
    showClara (ConditionalFuncApp tn args) =
        brackets (pretty (capitalise tn) <+> hsep (map (parens . showClara) args))
    showClara (ConditionalEval cOp arg1 arg2) = brackets (pretty ":test" <+> parens (showClara cOp <+> showClara arg1 <+> showClara arg2))
    showClara (ConditionalExist UBnot arg) = brackets (pretty ":not" <+> showClara arg)
    showClara (ConditionalElementFail err) = pretty $ "ConditionalElementFailure: " ++ show err

instance ShowDrools ConditionalElement where
    showDrools (ConditionalFuncApp tn args) =
        pretty (capitalise tn) <> parens (hsep $ punctuate comma $ map showDrools args)
    showDrools (ConditionalEval cOp arg1 arg2 ) = pretty "eval" <> parens (showDrools arg1 <+> showDrools cOp <+> showDrools arg2)
    showDrools (ConditionalExist UBnot arg) = pretty "not" <+> parens (showDrools arg)
    showDrools (ConditionalElementFail err) = pretty $ "ConditionalElementFailure: " ++ show err

instance ShowClara BComparOp where
    showClara BClt  = pretty "<"
    showClara BClte = pretty "<="
    showClara BCgt  = pretty ">"
    showClara BCgte = pretty ">="
    showClara BCne = pretty "!=" -- check with johsi
    showClara BCeq = pretty "="

instance ShowDrools BComparOp where
    showDrools BClt  = pretty "<"
    showDrools BClte = pretty "<="
    showDrools BCgt  = pretty ">"
    showDrools BCgte = pretty ">="
    showDrools BCne = pretty "!="
    showDrools BCeq = pretty "==" -- NOTE: Bindings in drools vs Equality in drools

instance ShowClara BArithOp where
    showClara BAadd = pretty "+"
    showClara BAsub = pretty "-"
    showClara BAmul = pretty "*"
    showClara BAdiv = pretty "/"
    showClara BAmod = pretty "%"

instance ShowDrools BArithOp where
    showDrools = showClara


data CEArg = CEBinding ProdVarName ProdFieldName
           | CEEquality ProdFieldName ProdVarName
           | CEArithmetic BArithOp CEArg CEArg
           | CEFuncApp ProdFuncName [ProdVarName]
           | CEVarExpr Argument
           | CELiteral Val
           | CEArgFail String
           deriving (Eq, Show)

instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> (colon <> pretty fv)
    showClara (CEBinding vn fn) = pretty "=" <+> pretty "?" <> pretty vn <+> pretty fn
    showClara (CEArithmetic aOp v1 v2) = lparen <> showClara aOp <+> showClara v1 <+> showClara v2 <> rparen
    showClara (CEFuncApp func vns) = parens (pretty func <+> hsep (punctuate comma (map ((<>) (pretty "?") . pretty) vns)))
    showClara (CELiteral x) = showClara x
    showClara (CEVarExpr vn) = pretty "?" <> showDrools vn
    showClara (CEArgFail err) = error $ "Transpilation failure: " ++ show err

instance ShowDrools CEArg where
    showDrools (CEEquality fn fv) = pretty fn <+> pretty "==" <+> squotes (pretty fv)
    showDrools (CEBinding vn fn) = pretty "$" <> pretty vn <+> pretty ":=" <+> pretty fn
    showDrools (CEArithmetic aOp v1 v2) = lparen <> showDrools v1 <+> showDrools aOp <+> showDrools v2 <> rparen
    showDrools (CEFuncApp func vns) = pretty func <> parens (hsep (punctuate comma (map ((<>) (pretty "$") . pretty) vns)))
    showDrools (CELiteral x) = showDrools x
    showDrools (CEVarExpr vn) = pretty "$" <> showDrools vn
    showDrools (CEArgFail err) = error $ "Transpilation failure: " ++ show err

instance ShowClara Val where
    showClara (StringV x) = colon <> pretty x
    showClara (BoolV x) = pretty x
    showClara (IntV x) = pretty x
    showClara (FloatV x) = pretty x
    showClara e = error $ "No Clara output defined for value: " ++ show e


instance ShowDrools Val where
    showDrools (StringV x) = squotes $ pretty x
    showDrools (BoolV x) = pretty x
    showDrools (IntV x) = pretty x
    showDrools (FloatV x) = pretty x
    showDrools e = error $ "No Drools output defined for value: " ++ show e

-- We restrict the format of the rules to a singular post condition (to support prolog-style syntax within l4)
data RuleAction = ActionFuncApp Typename ProductionRuleTrace [CEArg]
                | ActionExprErr String
                deriving (Eq, Show)

instance ShowClara RuleAction where
    showClara (ActionFuncApp fname _ args) = pretty "(c/insert! (->" <> pretty fname <+> hsep (map showClara args)
    showClara (ActionExprErr x) = pretty x

instance ShowDrools RuleAction where
    showDrools (ActionFuncApp fname tObj appArgs) =
          pretty "insertLogical(new"
      <+> pretty (capitalise fname)
      <>  parens (hsep (punctuate comma $ newJustif : fields ))
      <>  pretty ");"
      where fields =  map showDrools appArgs
            newJustif = pretty "new" <+> pretty (capitalise $ nameOfTraceObj tObj) <> parens (hsep $ punctuate comma (showPriors 0 (priors tObj) <> showArgs (args tObj)))
            showPriors acc (_:ps) = (pretty "$j" <> viaShow acc) : showPriors (acc+1) ps
            showPriors _ [] = []
            showArgs (TraceTup a:as) = pretty "$" <> pretty (fst a) : showArgs as
            showArgs [] = []
    showDrools (ActionExprErr x) = pretty x



data Argument = GlobVar ProdVarName | LocVar ProdVarName | Value String deriving (Eq, Show)

instance ShowClara Argument where
    showClara (GlobVar x)  = pretty x
    showClara (LocVar x)   = pretty x
    showClara (Value y)    = pretty y

instance ShowDrools Argument where
    showDrools (GlobVar x) = pretty x
    showDrools (LocVar x)  = pretty x
    showDrools (Value y)   = pretty y


yieldTp :: [Char] -> [Char]
yieldTp x = if x `elem` ["Justification", "Integer", "Boolean", "Float"] then x else "String"