module ToRules where

import Syntax (Program, Tp, Expr, Val)
import MainHelpers (getTpAst)

type Typename = String
type VariableName = String 
type FieldName = String

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

data ProductionClassField = ProductionClassField FieldName ProductionClassType
newtype ProductionClassType = ProductionClassType Typename

data ProductionRule = ProductionRule { nameOfProductionRule :: String 
                                     , varDeclsOfProductionRule :: [VariableName]
                                     , leftHandSide :: PreCondition
                                     , rightHandSide :: [Action]
                                     }

data PreCondition 
    = And PreCondition PreCondition 
    | Or PreCondition PreCondition 
    | Not PreCondition 
    | PreCondition ConditionalElement         

-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement 
    = ConditionalElement Typename [CEArg] 
    | ConditionalEval (Expr ())  -- TODO: flesh this out  

data CEArg = CEBinding VariableName FieldName | CEEquality FieldName Val

-- We restrict the format of the rules to a singular post condition (to support prolog-style syntax within l4)
data Action = InsertLogical Typename [Argument]
data Argument = Variable VariableName | Value Val

astToRules :: Program (Tp ()) -> IO ()
astToRules = undefined
