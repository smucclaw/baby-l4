{-# LANGUAGE NamedFieldPuns #-}
module ToRules where

import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import L4LSP (arNameToString)
import SimpleRules (isRule, condValid)
import Data.Either (lefts, rights)

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
        hang 2 $ vsep [ pretty "(defrule" <+> pretty nameOfProductionRule
                      , pretty "(preconds here)"
                      , pretty "=>"
                      , pretty "(postconds here)" <> pretty ")" <> PP.line
                      ]

instance ShowDrools ProductionRule where
    showDrools ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        vsep [ nest 2 $ vsep [pretty "rule" <+> pretty nameOfProductionRule
                             , pretty "when" 
                             , pretty "(preconds here)"
                             , pretty "then"
                             , pretty "(postconds here)"
                             ]
             , pretty "end" <> PP.line
             ]


data RuleCondition 
    = And RuleCondition RuleCondition 
    | Or RuleCondition RuleCondition 
    | Not RuleCondition 
    | RuleCondition ConditionalElement 
    deriving Show


-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement 
    = ConditionalElement Typename [CEArg] 
    | ConditionalEval (Expr ())  -- TODO: flesh this out  
    deriving Show

data CEArg = CEBinding ProdVarName ProdFieldName | CEEquality ProdFieldName Val deriving Show

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
                     , leftHandSide = RuleCondition $ ConditionalElement "" [CEBinding "" ""]
                     , rightHandSide = InsertLogical "" [Variable ""]
                     }

varDeclToProdVarName :: VarDecl t -> ProdVarName
varDeclToProdVarName = undefined

precondToRuleCondition :: Expr t -> RuleCondition
precondToRuleCondition = undefined

postcondToRuleAction :: Expr t -> RuleCondition
postcondToRuleAction = undefined


astToRules :: Program (Tp ()) -> IO ()
astToRules x = do
    let lrRules = map filterRule $ rulesOfProgram x
    print $ lefts lrRules
    putDoc $ vsep [ showClara $ head $ rights lrRules 
                  , showDrools $ head $ rights lrRules 
                  ]
