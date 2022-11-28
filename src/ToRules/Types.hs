{- | Intermediate AST to Rule Engine syntax conversion module.

This module contains the type declarations used within the intermediate AST encoding the various syntactical structures of Rule Engine formats.


TODOS:

(1:09112021)
Given that CE & FE expressions share such similar structure, it might be good for us to define some lower-level types that can generalize the similariites, and use CE and FE as wrappers
(1:08122021)
Refactor the program such that we produce the ProductionSystem ast first before pretty printing. Would likely require monad transformers.
-}
{-# LANGUAGE NamedFieldPuns #-}
module ToRules.Types where

import Prettyprinter
import L4.Syntax
import GHC.Utils.Misc (capitalise)
import GHC.Core (Tickish(profNoteCC))

-- L4 AST -> intermediate AST -> output format (clara/drools)

-- intermediate AST follows structure of a DRL file
-- ProductionRule is the drools rule data type

-- * Supported conversion formats
-- | Nullary constructor that represents various supported rule-engine output formats 
data RuleFormat = Clara
                  -- ^ [Clara-Rules](https://clara-rules.org)
                | Drools
                  -- ^ [Drools v7.58.0](https://docs.drools.org/7.58.0.Final/drools-docs/html_single/)
                deriving Eq

-- | Returns the appropriate ast pretty-printing function for a given RuleFormat.
showForm :: (ShowDrools x, ShowClara x) => RuleFormat -> (x -> Doc ann)
showForm rf = case rf of
            Drools -> showDrools
            Clara -> showClara


-- ** Prettyprinting Typeclasses
class ShowClara x where
    showClara :: x -> Doc ann

class ShowDrools x where
    showDrools :: x -> Doc ann

instance (ShowClara t) => ShowClara (Maybe t) where
    showClara (Just x) = showClara x
    showClara Nothing = pretty ""

instance (ShowDrools t) => ShowDrools (Maybe t) where
    showDrools (Just x) = showDrools x
    showDrools Nothing = pretty ""


-- * Types & Instances

-- ** General AST types
type Declname = String
type Typename = String
type ProdVarName = String
type ProdFieldName = String
type ProdFuncName = String
type ProdAnnot = String

type RCList = [ConditionalElement]
type RAList = [RuleAction]

{- | Represents a single program. 

Currently only function & class declarations ("ToRules.ToDecls"), and rules ("ToRules.ToRules") are fleshed out.
 
TODO: see (1:08122021)
-}
data ProductionSystem = ProductionSystem { boilerplate :: String
                                         , functions :: [ProductionDefn]
                                         , queries :: String
                                         , classDecls :: [ProductionClassDecl]
                                         , globals :: String
                                         , rules :: [ProductionRule]
                                         }

instance ShowClara ProductionSystem where
    showClara ProductionSystem {functions, classDecls, rules} =
        vsep [ pretty sysHead
             , vsep $ map showClara functions
             , line
             , vsep $ map showClara classDecls
             , line
             , vsep $ map showClara rules]
        where
            sysHead = "(ns transpiled\n  (:require\n   [clara.rules :as c :refer [defrule]]))\n"

instance ShowDrools ProductionSystem where
    showDrools ProductionSystem {functions, classDecls, rules} =
        vsep [ pretty sysHead
             , vsep $ map showDrools functions
             , line
             , vsep $ map showDrools classDecls
             , line
             , vsep $ map showDrools rules ]
        where
            sysHead = "package com.sample.transpiled\n\nimport java.util.Arrays\nimport java.util.ArrayList\n"


-- | Represents a single output function. 
data ProductionDefn = ProductionDefn { nameOfProductionDefn :: ProdFuncName
                                       -- ^ Name of the output function
                                     , returnTpOfProductionDefn :: Typename
                                       -- ^ Return type of the output function
                                     , argsOfProductionDefn :: [(ProdVarName, Typename)]
                                       -- ^ List of parameter names and their accompanying types
                                     , bodyOfProductionDefn :: ProdFuncExpr
                                       -- ^ Expression contained within the body of the output function
                                     } deriving (Eq, Show)

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


-- | Represents the various expressions that can be output within the body of ProductionDefn.
--   TODO: See point (1:09112021) above
data ProdFuncExpr = FEFuncApp ProdFuncName [ProdFuncExpr]
                    -- ^ Function Application
                  | FEComparison BComparOp ProdFuncExpr ProdFuncExpr
                    -- ^ Binary Comparisons (as supported by native L4)
                  | FEArithmetic BArithOp ProdFuncExpr ProdFuncExpr
                    -- ^ Binary Arithmetic Expressions (as supported by native L4)
                  | FEVarExpr Argument
                    -- ^ Local Parameter 
                  | FELiteral Val
                    -- ^ Literal value (as supported by native L4)
                  | ProdFuncExprFail String
                    -- ^ Failure to compile
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

-- | Represents a single output class declaration.
data ProductionClassDecl = ProductionClassDecl { nameOfProductionClassDecl :: Declname
                                                 -- ^ Name of class declaration
                                               , fieldsOfProductionClassDecl :: [ProductionClassField]
                                                 -- ^ Class variables 
                                               } deriving Show

instance ShowClara ProductionClassDecl where
    showClara ProductionClassDecl {nameOfProductionClassDecl, fieldsOfProductionClassDecl} = do
        parens (pretty "defrecord" <+> pretty (capitalise nameOfProductionClassDecl) <+> brackets (hsep $ map showClara fieldsOfProductionClassDecl))

instance ShowDrools ProductionClassDecl where
    showDrools ProductionClassDecl {nameOfProductionClassDecl, fieldsOfProductionClassDecl} = do
        vsep [ pretty "declare" <+> pretty (capitalise nameOfProductionClassDecl)
             , indent 2 $ vsep (map showDrools fieldsOfProductionClassDecl)
             , pretty "end" <> line
             ]

-- | Represents a single output class variable. "ProdFieldName" is the name of the variable, "Typename" is the type of the variable, and "PosAnnot" is a string annotation that encodes positional information of the variable within the class constructor.
data ProductionClassField = ProductionClassField ProdFieldName Typename PosAnnot deriving Show
type PosAnnot = String -- positional annotation

instance ShowDrools ProductionClassField where
    showDrools (ProductionClassField pfname tpname pos) = pretty pfname <> colon <+> pretty (yieldTp tpname) <+> pretty "@position" <> parens (pretty pos)

instance ShowClara ProductionClassField where
    showClara (ProductionClassField pfname _ _) = pretty pfname



-- | Represents a single production rule. 
data ProductionRule =
    ProductionRule { nameOfProductionRule :: String
                     -- ^ Name of the rule
                   , varDeclsOfProductionRule :: [ProdVarName]
                     -- ^ Locally defined variables, used within the rule itself
                   , leftHandSide :: RCList -- RuleCondition
                     -- ^ Preconditions of rule
                   , rightHandSide :: RuleAction
                     -- ^ Singular resulting post-condition of rule
                   , traceObj :: ProductionRuleTrace
                     -- ^ Justification object associated with an instance of the firing of this rule
                   } deriving Show

instance ShowClara ProductionRule where
    showClara ProductionRule {nameOfProductionRule, leftHandSide, rightHandSide, traceObj} = do
        vsep [ pretty "" -- showClara traceObj
             , hang 2 $ vsep [ lparen <> pretty "defrule" <+> pretty nameOfProductionRule
                             , vsep (map showClara leftHandSide)
                             , pretty "=>"
                             , showClara rightHandSide <> rparen <> line -- stylisticly, lisps have no dangling brackets
                             ]]

instance ShowDrools ProductionRule where
    showDrools ProductionRule {nameOfProductionRule, leftHandSide, rightHandSide, traceObj} = do
        vsep [ nest 2 $ vsep [pretty "rule" <+> dquotes (pretty nameOfProductionRule)
                             , pretty "when"
                             , indent 2 $ vsep (map showDrools leftHandSide)
                             , pretty "then"
                             , indent 2 $ showDrools rightHandSide
                             ]
             , pretty "end" <> line
             , showDrools traceObj <> line
             ]


{- | Represents a single precondition.

The "Maybe RCBind" in "ConditionalFuncApp" represents a pattern binding.
-}
data ConditionalElement
    = ConditionalFuncApp (Maybe RCBind) ProdFuncName [CEArg]
    | ConditionalEval BComparOp CEArg CEArg
    | ConditionalNegation UBoolOp ConditionalElement -- ^ Only "UBnot" is supported 
    | ConditionalElementFail String -- when l4 rule does not contain enough info to be converted to drools/clara form
    -- e.g. vars in not that aren't previously bound
    deriving (Eq, Show)

newtype RCBind = RCBind String deriving (Eq)

instance Show RCBind where
    show (RCBind x) = x

instance ShowClara RCBind where
    showClara (RCBind s) = pretty "?" <> pretty s <+> pretty "<- "
instance ShowDrools RCBind where
    showDrools (RCBind s) = pretty "$" <> pretty s <+> pretty ": "

instance ShowClara ConditionalElement where
    showClara (ConditionalFuncApp (Just b@(RCBind bn)) tn args) =
        brackets $ hang 2 $ vsep [ showClara b <> pretty (capitalise tn) <+> brackets (deconstruct args)
                                 , vsep (map (parens . showClara) args)
                                 ]
        where deconstruct as = lbrace <> brackets (hsep (map ((<>) (pretty bn) . tear) as)) <+> pretty ":args" <> rbrace
              tear = pretty . last . ceArgName 
    showClara (ConditionalFuncApp _ tn args) =
        hang 2 $ brackets $ vsep [ pretty (capitalise tn) <+> brackets (deconstruct args)
                                 , vsep (map (parens . showClara) args)
                      ]
        where deconstruct as = lbrace <> brackets (hsep (map (pretty . ceArgName) as)) <+> pretty ":args" <> rbrace
    showClara (ConditionalEval cOp arg1 arg2) = brackets (pretty ":test" <+> parens (showClara cOp <+> showClara arg1 <+> showClara arg2))
    showClara (ConditionalNegation UBnot arg) = brackets (pretty ":not" <+> showClara arg)
    showClara (ConditionalElementFail err) = pretty $ "ConditionalElementFailure: " ++ show err

instance ShowDrools ConditionalElement where
    showDrools (ConditionalFuncApp bn tn args) =
        showDrools bn <> pretty (capitalise tn) <> parens (hsep $ punctuate comma $ map showDrools args)
    showDrools (ConditionalEval cOp arg1 arg2 ) = pretty "eval" <> parens (showDrools arg1 <+> showDrools cOp <+> showDrools arg2)
    showDrools (ConditionalNegation UBnot arg) = pretty "not" <+> parens (showDrools arg)
    showDrools (ConditionalElementFail err) = pretty $ "ConditionalElementFailure: " ++ show err

instance ShowClara BComparOp where
    showClara BClt  = pretty "<"
    showClara BClte = pretty "<="
    showClara BCgt  = pretty ">"
    showClara BCgte = pretty ">="
    showClara BCne = pretty "!="
    showClara BCeq = pretty "="

instance ShowDrools BComparOp where
    showDrools BCeq = pretty "==" -- NOTE: Bindings in drools vs Equality in drools
    showDrools x = showClara x

instance ShowClara BArithOp where
    showClara BAadd = pretty "+"
    showClara BAsub = pretty "-"
    showClara BAmul = pretty "*"
    showClara BAdiv = pretty "/"
    showClara BAmod = pretty "%"

instance ShowDrools BArithOp where
    showDrools = showClara


-- | Represents the various expressions that can be output within the body of "ConditionalElement". TODO: See (1:09112021) above.
data CEArg = CEBinding ProdVarName ProdFieldName
           | CEEquality ProdFieldName ProdVarName
           | CEArithmetic BArithOp CEArg CEArg
           | CEFuncApp ProdFuncName [ProdVarName]
           | CEVarExpr Argument
           | CELiteral Val
           | CEArgFail String
           deriving (Eq, Show)

-- | Extracts the name of a CEArg 
ceArgName :: CEArg -> String
ceArgName (CEBinding _ pvn) = pvn
ceArgName (CEEquality pfn _) = pfn
ceArgName (CEFuncApp fn _) = fn
ceArgName (CEVarExpr a) = show a 
ceArgName (CELiteral v) = show v
ceArgName x = error $ "Failed to extract name of " ++ show x


instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> dquotes (pretty fv)
    showClara (CEBinding vn fn) = pretty "=" <+> pretty fn <+> pretty "?" <> pretty vn
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
    showClara (StringV x) = dquotes $ pretty x
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
data RuleAction = ActionFuncApp Typename ProductionRuleTrace [CEArg] -- ^ Represents the positive logical literal that results from the fulfilment of a rule
                | ActionExprErr String  -- ^ TODO: Handle errors seperately, related to (1:08122021).
                deriving (Eq, Show)

-- | Helper function for prettyprinting local variable bindings within justification object instances
showArgs :: Char -> [TraceTuple] -> [Doc ann]
showArgs chr (TraceTup a:as) = (pretty chr <> pretty (fst a)) : showArgs chr as
showArgs _ [] = []

-- | Helper function for prettyprinting precondition predicate bindings within justification object instances
showPriors :: Char -> Integer -> [TraceTuple] -> [Doc ann]
showPriors chr acc (_:ps) = (pretty chr <> pretty "j" <> viaShow acc) : showPriors chr (acc+1) ps
showPriors _ _ [] = []

instance ShowClara RuleAction where
    showClara (ActionFuncApp fname tObj appArgs) =
            pretty "(c/insert! (->"
        <>  pretty (capitalise fname)
        <+> showTrace tObj
        <+> dquotes (pretty $ capitalise fname)
        <+> dquotes (pretty "pred")
        <+> brackets (hsep (map showClara appArgs))
        <>  rparen <> rparen
        where showTrace trc = brackets (
                   lbrace
                <> pretty ":name" <+> dquotes (pretty $ rulename trc)
                <+> pretty ":type" <+> dquotes (pretty "rule")
                <+> pretty ":args" <+> brackets (hsep (showArgs '?' (args trc)))
                <+> pretty ":trace" <+> brackets (hsep (showPriors '?' 0 (priors trc)))
                <> rbrace
                )
    showClara (ActionExprErr x) = pretty x

instance ShowDrools RuleAction where
    showDrools (ActionFuncApp fname tObj appArgs) =
          pretty "insertLogical(new"
      <+> pretty (capitalise fname)
      <>  parens (hsep (punctuate comma $ newJustif : fields ))
      <>  pretty ");"
      where fields =  map showDrools appArgs
            newJustif = pretty "new" <+> pretty (capitalise $ nameOfTraceObj tObj) <> parens (hsep $ punctuate comma (showPriors '$' 0 (priors tObj) <> showArgs '$' (args tObj)))
    showDrools (ActionExprErr x) = pretty x


-- | Represents an atomic expression within the AST
data Argument = GlobVar ProdVarName -- ^ Variable defined in global scope of the program
              | LocVar ProdVarName  -- ^ Variable defined in local scope of a rule expression
              | Value String        -- ^ Literal value, as supported by L4 syntax
              deriving (Eq, Show)

instance ShowClara Argument where
    showClara (GlobVar x)  = pretty x
    showClara (LocVar x)   = pretty x
    showClara (Value y)    = pretty y

instance ShowDrools Argument where
    showDrools (GlobVar x) = pretty x
    showDrools (LocVar x)  = pretty x
    showDrools (Value y)   = pretty y


-- ** Justification types
-- | Represents a single justification class declaration, which is used to generate a trace for rule-firing.
data ProductionRuleTrace = ProductionRuleTrace { nameOfTraceObj :: String -- ^ Name of the trace object
                                               , rulename :: String       -- ^ Name of the associated rule
                                               , priors :: [TraceTuple]   -- ^ The predicates that fulfilled the preconditions of, and led to firing of the associated rule
                                               , args :: [TraceTuple]     -- ^ The values of locally bound variables, when the associated rule fired.
                                               } deriving (Eq, Show)

newtype TraceTuple = TraceTup (ProdFieldName, Typename) deriving (Eq, Show)

-- | Helper function that 
priorsAndArgs :: Integer -> Integer -> [TraceTuple] -> [(String, Typename)]
priorsAndArgs pAcc aAcc (TraceTup x:xs) = case x of
    (_, "Justification") -> ("prior" ++ show pAcc, "Object") : priorsAndArgs (pAcc + 1) aAcc xs
    (_, y) -> ("arg" ++ show aAcc, y) : priorsAndArgs pAcc (aAcc + 1) xs
priorsAndArgs _ _ [] = []

-- | Helper function that converts tracetuples into "ProductionClassField" for use in prettyprinting
traceTupsToProdClassFields :: Integer -> [(String, Typename)] -> [ProductionClassField]
traceTupsToProdClassFields cnt ((x,y):tps)  = ProductionClassField x y (show cnt) : traceTupsToProdClassFields (cnt+1) tps
traceTupsToProdClassFields _ [] = []

-- | Helper function that filters for allowable types in Drools output (Clara output is not typed)
yieldTp :: [Char] -> [Char]
yieldTp x = if x `elem` ["Object", "Justification", "Integer", "Boolean", "Float"] then x else "String"

instance ShowClara ProductionRuleTrace where
    showClara ProductionRuleTrace { nameOfTraceObj, priors, args} =
        showClara ProductionClassDecl { nameOfProductionClassDecl = nameOfTraceObj
                                      , fieldsOfProductionClassDecl = traceTupsToProdClassFields 0 claraPriArgs}
        where claraPriArgs = case priorsAndArgs 0 0 (priors <> args) of
                                [] -> ("fact", "") : []
                                x  -> ("rule", "") : x

instance ShowDrools ProductionRuleTrace where
    showDrools ProductionRuleTrace {nameOfTraceObj, priors, args} =
        showDrools ProductionClassDecl { nameOfProductionClassDecl = nameOfTraceObj ++ " extends Justification"
                                       , fieldsOfProductionClassDecl = traceTupsToProdClassFields 0 (priorsAndArgs 0 0 $ priors <> args)}
