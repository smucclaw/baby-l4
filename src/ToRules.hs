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
                             , pretty "(preconds here)"
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
                                                          , showClara right]
    showClara (Or left right) = brackets $ nest 2 $ vsep [ pretty ":or"
                                                         , showClara left -- fix, align left & right column-wise
                                                         , showClara right]
    showClara (Not rc) = brackets (pretty ":not" <+> showClara rc)
    showClara (RuleCondition ce) = showClara ce

-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement
    = ConditionalElement Typename [CEArg]
    | ConditionalEval (Expr ())  -- TODO: flesh this out  
    deriving Show

instance ShowClara ConditionalElement where
    showClara (ConditionalElement tn args) =
        brackets (pretty (capitalize tn) <+> (hsep (map (parens . showClara) args)))
    showClara (ConditionalEval expr) = pretty "ConditionalEval not implemented yet"

-- TODO: Figure out what the replacement for String in CEEquality should be
--          Potential alternatives: OkT? ClsNm?
--       *To ask Martin*
data CEArg = CEBinding ProdVarName ProdFieldName | CEEquality ProdFieldName String deriving Show

instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> pretty fv
    showClara (CEBinding vn fn) = pretty "=" <+> pretty "?" <> pretty vn <+> pretty fn

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
precondToRuleCondition fApp@AppE {} =
    let (fname, args) = appToFunArgs [] fApp
    in RuleCondition $ exprToConditionalElement fname args
precondToRuleCondition (BinOpE _ binOp arg1 arg2)
    | binOp == BBool BBand = And (precondToRuleCondition arg1) (precondToRuleCondition arg2)
    | binOp == BBool BBor = Or (precondToRuleCondition arg1) (precondToRuleCondition arg2)
precondToRuleCondition (UnaOpE _ unaOp arg)
    | unaOp == UBool UBnot = Not (precondToRuleCondition arg)
precondToRuleCondition _ = error "beep boop"

getName :: Expr t -> VarName
getName = nameOfQVarName . nameOfVar . varOfExprVarE

exprToConditionalElement :: Expr t -> [Expr t] -> ConditionalElement
exprToConditionalElement func@(VarE _ _) args = ConditionalElement (getName func) (map exprToCEArg $ zip [0.. ((length args)-1)] args)
exprToConditionalElement _ _ = error "tbd"

defArg :: Int -> ProdFieldName
defArg x = "arg" ++ show x

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
    putDoc $ showClara (gdRules !! 5)
