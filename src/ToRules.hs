{-# LANGUAGE NamedFieldPuns #-}
module ToRules where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Syntax
import L4LSP (arNameToString)
import SimpleRules (isRule, condValid)
import Data.Either (lefts, rights)
import Data.Char (toUpper)

data RuleFormat = Clara | Drools deriving Eq

type Typename = String
type ProdVarName = String
type ProdFieldName = String
type ProdFuncName = String

class ShowClara x where
    showClara :: x -> Doc ann

class ShowDrools x where
    showDrools :: x -> Doc ann

-- anythin marked string is non-fleshed
data ProductionSystem = ProductionSystem { package :: String
                                         , imports :: String
                                         , functions :: String
                                         , queries :: String
                                         , classDecls :: [ProductionClassDecl]
                                         , globals :: String
                                         , rules :: [ProductionRule]
                                         }

-- declare Person
--     name: String 
--     age: Integer 
-- end

data ProductionClassDecl = ProductionClassDecl { nameOfProductionClassDecl :: Typename
                                               , fieldsOfProductionClassDecl :: [ProductionClassField]
                                               }

data ProductionClassField = ProductionClassField ProdFieldName ProductionClassType
newtype ProductionClassType = ProductionClassType Typename


data ProductionRule = ProductionRule { nameOfProductionRule :: String
                                     , varDeclsOfProductionRule :: [ProdVarName]
                                     , leftHandSide :: RuleCondition
                                     , rightHandSide :: RuleAction
                                     } deriving Show

instance ShowClara ProductionRule where
    showClara ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        hang 2 $ vsep [ lparen <> pretty "defrule" <+> pretty nameOfProductionRule
                      , showClara leftHandSide
                      , pretty "=>"
                      , pretty "(postconds here)" <> rparen <> line -- stylisticly, lisps have no dangling brackets
                      ]

instance ShowDrools ProductionRule where
    showDrools ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        vsep [ nest 2 $ vsep [pretty "rule" <+> dquotes (pretty nameOfProductionRule)
                             , pretty "when"
                             , indent 2 $ showDrools leftHandSide
                             , pretty "then"
                             , pretty "(postconds here)"
                             ]
             , pretty "end" <> line
             ]


data RuleCondition
    = And RuleCondition RuleCondition
    | Or RuleCondition RuleCondition
    | Not RuleCondition
    | RuleCondition ConditionalElement
    deriving Show

instance ShowClara RuleCondition where
    showClara (And left right) = brackets $ nest 2 $ vsep [ pretty ":and"
                                                          , showClara left
                                                          , showClara right ]
    showClara (Or left right) = brackets $ nest 2 $ vsep [ pretty ":or"
                                                         , showClara left -- fix, align left & right column-wise
                                                         , showClara right ]
    showClara (Not rc) = brackets (pretty ":not" <+> showClara rc)
    showClara (RuleCondition ce) = showClara ce

instance ShowDrools RuleCondition where
    showDrools (And left right) = vsep [ showDrools left
                                       , showDrools right ]
    showDrools (Or left right) = parens $ pretty "or" <+> showDrools left <+> showDrools right
    showDrools (Not rc) = pretty "not" <+> showDrools rc
    showDrools (RuleCondition ce) = showDrools ce

-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement
    = ConditionalFuncApp Typename [CEArg]
    | ConditionalEval BComparOp CEArg CEArg-- TODO: flesh this out  
    deriving Show

cOpToStr :: BComparOp -> String
cOpToStr BClt  = "<"
cOpToStr BClte = "<="
cOpToStr BCgt  = ">"
cOpToStr BCgte = ">="
cOpToStr BCne = "!=" -- check with johsi
cOpToStr BCeq = "=="

instance ShowClara ConditionalElement where
    showClara (ConditionalFuncApp tn args) =
        brackets (pretty (capitalize tn) <+> hsep (map (parens . showClara) args))
    showClara (ConditionalEval cOp arg1 arg2) = brackets (pretty ":test" <+> parens (pretty (cOpToStr cOp) <+> showClara arg1 <+> showClara arg2))
    -- showClara (ConditionalVarExpr _) = pretty ""

instance ShowDrools ConditionalElement where
    showDrools (ConditionalFuncApp tn args) =
        pretty (capitalize tn) <+> parens (hsep $ punctuate comma $ map showDrools args)
    showDrools (ConditionalEval _ _ _ ) = pretty "ConditionalEval not implemented yet"
    -- showDrools (ConditionalVarExpr _) = pretty "ConditionalFuncApp not implemented yet"

-- TODO: Figure out what the replacement for String in CEEquality should be
--          Potential alternatives: OkT? ClsNm?
--       *To ask Martin*
data CEArg = CEBinding ProdVarName ProdFieldName
           | CEEquality ProdFieldName String
           | CEFuncApp ProdFuncName [ProdVarName]
           | CEVarExpr ProdVarName
           deriving Show

instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> pretty fv
    showClara (CEBinding vn fn) = pretty "=" <+> pretty "?" <> pretty vn <+> pretty fn
    showClara (CEFuncApp func vns) = pretty func <> parens (hsep (punctuate comma (map ((<>) (pretty "?") . pretty) vns)))
    showClara (CEVarExpr vn) = pretty "?" <> pretty vn

instance ShowDrools CEArg where
    showDrools (CEEquality fn fv) = pretty fn <+> pretty "==" <+> pretty fv
    showDrools (CEBinding vn fn) = pretty "$" <> pretty vn <> pretty ":" <+> pretty fn
    showDrools (CEFuncApp func vns) = pretty ""

-- We restrict the format of the rules to a singular post condition (to support prolog-style syntax within l4)
data RuleAction = InsertLogical Typename [Argument] deriving Show
data Argument = Variable ProdVarName | Value Val deriving Show

filterRule :: Rule t -> Either String ProductionRule
filterRule x
    | isRule x = Right $ ruleToProductionRule x
    | otherwise = Left $ "Not a valid rule: " ++ arNameToString (nameOfRule x) ++ "\n"

ruleToProductionRule :: Rule t -> ProductionRule
ruleToProductionRule Rule {nameOfRule, varDeclsOfRule, precondOfRule, postcondOfRule}
    = ProductionRule { nameOfProductionRule = arNameToString nameOfRule
                     , varDeclsOfProductionRule = [""]
                     , leftHandSide = precondToRuleCondition precondOfRule
                     , rightHandSide = InsertLogical "" [Variable ""]
                     }

varDeclToProdVarName :: VarDecl t -> ProdVarName
varDeclToProdVarName = undefined

capitalize :: String -> String
capitalize xs = toUpper (head xs) : tail xs

-- remove this after merging to main (add "import SyntaxManipulations (appToFunArgs")
appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])
appToFunArgs acc (AppE _ f a) = appToFunArgs (a:acc) f
appToFunArgs acc t = (t, acc)

precondToRuleCondition :: Expr t -> RuleCondition
precondToRuleCondition fApp@AppE {} = RuleCondition $ exprToConditionalFuncApp fApp
precondToRuleCondition compOp@(BinOpE _ (BCompar _) _ _) = RuleCondition $ exprToConditionalEval compOp
precondToRuleCondition (BinOpE _ (BBool binOp) arg1 arg2)
    | binOp == BBand = And (precondToRuleCondition arg1) (precondToRuleCondition arg2)
    | binOp == BBor = Or (precondToRuleCondition arg1) (precondToRuleCondition arg2)
precondToRuleCondition (UnaOpE _ unaOp arg)
    | unaOp == UBool UBnot = Not (precondToRuleCondition arg)
precondToRuleCondition _ = error "beep boop"

getName :: Expr t -> VarName
getName = nameOfQVarName . nameOfVar . varOfExprVarE

exprToConditionalFuncApp :: Expr t -> ConditionalElement
exprToConditionalFuncApp fApp@AppE {} =
    let (fexpr, args) = appToFunArgs [] fApp
    in ConditionalFuncApp (getName fexpr) (map (exprToCEArg) $ zip [0.. ((length args)-1)] args)
exprToConditionalFuncApp _ = error "exprToConditionalFuncApp used for non-AppE"

exprToConditionalEval :: Expr t -> ConditionalElement
exprToConditionalEval (BinOpE _ (BCompar bop) x@AppE {} y@AppE {}) =
    ConditionalEval bop (exprToCEFuncApp x) (exprToCEFuncApp y)
exprToConditionalEval (BinOpE _ (BCompar bop) x@AppE {} y          ) =
    ConditionalEval bop (exprToCEFuncApp x) (CEVarExpr $ getName y)
exprToConditionalEval (BinOpE _ (BCompar bop) x           y@AppE {}) = ConditionalEval bop (CEVarExpr $ getName x) (exprToCEFuncApp y)
exprToConditionalEval (BinOpE _ (BCompar bop) x           y          ) = ConditionalEval bop (CEVarExpr $ getName x) (CEVarExpr $ getName y)
exprToConditionalEval _ = error "exprToConditionalEval used for non-BComparOp"

defArg :: Int -> ProdFieldName
defArg x = "arg" ++ show x

exprToCEFuncApp :: Expr t -> CEArg
exprToCEFuncApp fApp@AppE {} =
    let (fexpr, args) = appToFunArgs [] fApp
    in CEFuncApp (getName fexpr) (map getName args)
exprToCEFuncApp _ = error "exprToCEFuncApp received non-AppE expr"

exprToCEArg :: (Int, Expr t) -> CEArg -- assumption: either local or global var expr
exprToCEArg (num, VarE _ (LocalVar name _)) = CEBinding (nameOfQVarName name) (defArg num)
exprToCEArg (num, VarE _ (GlobalVar name)) = CEEquality (defArg num) (nameOfQVarName name)
exprToCEArg _ = error "nope"

postcondToRuleAction :: Expr t -> RuleCondition
postcondToRuleAction = undefined


astToRules :: Program (Tp ()) -> IO ()
astToRules x = do
    let lrRules = map filterRule $ rulesOfProgram x
        gdRules = rights lrRules
    print $ lefts lrRules
    print $ head $ gdRules
    putStrLn ""
    putStrLn "Clara:"
    putDoc $ showClara (gdRules !! 5)
    putStrLn ""
    -- putStrLn "Drools:"
    -- putDoc $ showDrools (gdRules !! 5)
