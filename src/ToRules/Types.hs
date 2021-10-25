{-# LANGUAGE NamedFieldPuns #-}
module ToRules.Types where

import Prettyprinter
import Syntax
import Util (capitalise)

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
                                         , functions :: String
                                         , queries :: String
                                         , classDecls :: [ProductionClassDecl]
                                         , globals :: String
                                         , rules :: [ProductionRule]
                                         }
data ProductionClassDecl = ProductionClassDecl { nameOfProductionClassDecl :: Typename
                                               , fieldsOfProductionClassDecl :: [ProductionClassField]
                                               } deriving Show
instance ShowDrools ProductionClassDecl where
    showDrools ProductionClassDecl {nameOfProductionClassDecl, fieldsOfProductionClassDecl} = do
        vsep [ pretty "declare" <+> pretty (capitalise nameOfProductionClassDecl)
             , indent 2 $ vsep (map showDrools fieldsOfProductionClassDecl)    
             , pretty "end"
             ]

data ProductionClassField = ProductionClassField ProdFieldName Typename deriving Show

instance ShowDrools ProductionClassField where
    showDrools (ProductionClassField pfname tpname) = pretty pfname <> colon <+> pretty tpname <+> pretty "@position" <> parens (pretty $ last pfname)



data ProductionRule =
    ProductionRule { nameOfProductionRule :: String
                   , varDeclsOfProductionRule :: [ProdVarName]
                   , leftHandSide :: RCList -- RuleCondition
                   , rightHandSide :: RAList 
                   } deriving Show

instance ShowClara ProductionRule where
    showClara ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        hang 2 $ vsep [ lparen <> pretty "defrule" <+> pretty nameOfProductionRule
                      , vsep (map showClara leftHandSide)
                    --   , showClara leftHandSide
                      , pretty "=>"
                      , vsep (map showClara rightHandSide) <> rparen <> line -- stylisticly, lisps have no dangling brackets
                      ]

instance ShowDrools ProductionRule where
    showDrools ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        vsep [ nest 2 $ vsep [pretty "rule" <+> dquotes (pretty nameOfProductionRule)
                             , pretty "when"
                            --  , indent 2 $ showDrools leftHandSide
                             , indent 2 $ vsep (map showDrools leftHandSide)
                             , pretty "then"
                             , indent 2 $ vsep (map showDrools rightHandSide)
                             ]
             , pretty "end" <> line
             ]



-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement
    = ConditionalFuncApp Typename [CEArg]
    | ConditionalEval BComparOp CEArg CEArg
    | ConditionalExist UBoolOp ConditionalElement -- TODO: rename to Negated Literal
    | ConditionalElementFail String
    deriving (Eq, Show)

instance ShowClara ConditionalElement where
    showClara (ConditionalFuncApp tn args) =
        brackets (pretty (capitalise tn) <+> hsep (map (parens . showClara) args))
    showClara (ConditionalEval cOp arg1 arg2) = brackets (pretty ":test" <+> parens (showClara cOp <+> showClara arg1 <+> showClara arg2))
    showClara (ConditionalExist UBnot arg) = brackets (pretty ":not" <+> showClara arg)
    showClara (ConditionalElementFail err) = error $ "ConditionalElementFailure: " ++ show err

instance ShowDrools ConditionalElement where
    showDrools (ConditionalFuncApp tn args) =
        pretty (capitalise tn) <> parens (hsep $ punctuate comma $ map showDrools args)
    showDrools (ConditionalEval cOp arg1 arg2 ) = pretty "eval" <> parens (showDrools arg1 <+> showDrools cOp <+> showDrools arg2)
    showDrools (ConditionalExist UBnot arg) = pretty "not" <+> parens (showDrools arg)
    showDrools (ConditionalElementFail err) = error $ "ConditionalElementFailure: " ++ show err
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
    showDrools BCeq = pretty ":=" -- NOTE: Bindings in drools vs Equality in drools

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
           | CEVarExpr ProdVarName 
           | CELiteral Val 
           | CEArgFail String
           deriving (Eq, Show)

instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> pretty fv
    showClara (CEBinding vn fn) = pretty "=" <+> pretty "?" <> pretty vn <+> pretty fn
    showClara (CEArithmetic aOp v1 v2) = lparen <> showClara aOp <+> showClara v1 <+> showClara v2 <> rparen
    showClara (CEFuncApp func vns) = parens (pretty func <+> hsep (punctuate comma (map ((<>) (pretty "?") . pretty) vns)))
    showClara (CELiteral x) = showClara x
    showClara (CEVarExpr vn) = pretty "?" <> pretty vn
    showClara (CEArgFail err) = error $ "Transpilation failure: " ++ show err 

instance ShowDrools CEArg where
    showDrools (CEEquality fn fv) = pretty fn <+> pretty "==" <+> squotes (pretty fv)
    showDrools (CEBinding vn fn) = pretty "$" <> pretty vn <+> pretty ":=" <+> pretty fn
    showDrools (CEArithmetic aOp v1 v2) = lparen <> showDrools v1 <+> showDrools aOp <+> showDrools v2 <> rparen
    showDrools (CEFuncApp func vns) = pretty func <> parens (hsep (punctuate comma (map ((<>) (pretty "$") . pretty) vns)))
    showDrools (CELiteral x) = showDrools x
    showDrools (CEVarExpr vn) = pretty "$" <> pretty vn
    showDrools (CEArgFail err) = error $ "Transpilation failure: " ++ show err 

instance ShowClara Val where
    showClara (StringV x) = squotes $ pretty x
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
data RuleAction = ActionFuncApp Typename [CEArg] 
                | ActionExprErr String
                deriving (Eq, Show)

instance ShowClara RuleAction where
    showClara (ActionFuncApp fname args) = pretty "(c/insert! (->" <> pretty fname <+> hsep (map ((<>) (pretty ":") . showClara) args)
    showClara (ActionExprErr x) = pretty x

instance ShowDrools RuleAction where
    showDrools (ActionFuncApp fname args) = pretty "insertLogical(new" <+> pretty (capitalise fname) <> parens ( hsep (punctuate comma $ map showDrools args)) <> pretty ");"
    showDrools (ActionExprErr x) = pretty x


data Argument = Variable ProdVarName | Value String deriving (Eq, Show)

instance ShowClara Argument where
    showClara (Variable x) = pretty x
    showClara (Value y)    = pretty y

instance ShowDrools Argument where
    showDrools (Variable x) = pretty x
    showDrools (Value y)    = pretty y
