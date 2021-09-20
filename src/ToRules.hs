{-# LANGUAGE NamedFieldPuns #-}
module ToRules where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Syntax
import L4LSP (arNameToString)
import SimpleRules (isRule, condValid)
import Data.Either (lefts, rights)
import Data.Char (toUpper)
import qualified Data.Set as S

data RuleFormat = Clara | Drools deriving Eq

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

type RCList = [ConditionalElement]

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
                                               }
data ProductionClassField = ProductionClassField ProdFieldName ProductionClassType
newtype ProductionClassType = ProductionClassType Typename


data ProductionRule =
    ProductionRule { nameOfProductionRule :: String
                   , varDeclsOfProductionRule :: [ProdVarName]
                   , leftHandSide :: RCList -- RuleCondition
                   , rightHandSide :: RuleAction
                   } deriving Show

instance ShowClara ProductionRule where
    showClara ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        hang 2 $ vsep [ lparen <> pretty "defrule" <+> pretty nameOfProductionRule
                      , vsep (map showClara leftHandSide)
                    --   , showClara leftHandSide
                      , pretty "=>"
                      , pretty "(postconds here)" <> rparen <> line -- stylisticly, lisps have no dangling brackets
                      ]

instance ShowDrools ProductionRule where
    showDrools ProductionRule {nameOfProductionRule, varDeclsOfProductionRule, leftHandSide, rightHandSide} = do
        vsep [ nest 2 $ vsep [pretty "rule" <+> dquotes (pretty nameOfProductionRule)
                             , pretty "when"
                            --  , indent 2 $ showDrools leftHandSide
                             , indent 2 $ vsep (map showDrools leftHandSide)
                             , pretty "then"
                             , pretty "(postconds here)"
                             ]
             , pretty "end" <> line
             ]



-- We are restricting ourselves to non-fact bindings 
-- (see https://docs.jboss.org/drools/release/7.58.0.Final/drools-docs/html_single/index.html#drl-rules-WHEN-con_drl-rules)
data ConditionalElement
    = ConditionalFuncApp Typename [CEArg]
    | ConditionalEval BComparOp CEArg CEArg
    deriving Show

instance ShowClara ConditionalElement where
    showClara (ConditionalFuncApp tn args) =
        brackets (pretty (capitalize tn) <+> hsep (map (parens . showClara) args))
    showClara (ConditionalEval cOp arg1 arg2) = brackets (pretty ":test" <+> parens (showClara cOp <+> showClara arg1 <+> showClara arg2))

instance ShowDrools ConditionalElement where
    showDrools (ConditionalFuncApp tn args) =
        pretty (capitalize tn) <> parens (hsep $ punctuate comma $ map showDrools args)
    showDrools (ConditionalEval cOp arg1 arg2 ) = pretty "eval" <> parens (showDrools arg1 <+> showDrools cOp <+> showDrools arg2)

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



data CEArg = CEBinding ProdVarName ProdFieldName
           | CEEquality ProdFieldName ProdVarName
           | CEFuncApp ProdFuncName [ProdVarName]
           | CEVarExpr ProdVarName
           deriving Show

instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> pretty fv
    showClara (CEBinding vn fn) = pretty "=" <+> pretty "?" <> pretty vn <+> pretty fn
    showClara (CEFuncApp func vns) = parens (pretty func <+> hsep (punctuate comma (map ((<>) (pretty "?") . pretty) vns)))
    showClara (CEVarExpr vn) = pretty "?" <> pretty vn

instance ShowDrools CEArg where
    showDrools (CEEquality fn fv) = pretty fn <+> pretty "==" <+> pretty fv
    showDrools (CEBinding vn fn) = pretty "$" <> pretty vn <+> pretty ":=" <+> pretty fn
    showDrools (CEFuncApp func vns) = pretty func <> parens (hsep (punctuate comma (map ((<>) (pretty "$") . pretty) vns)))
    showDrools (CEVarExpr vn) = pretty "$" <> pretty vn

-- We restrict the format of the rules to a singular post condition (to support prolog-style syntax within l4)
data RuleAction = InsertLogical Typename [Argument] deriving Show
data Argument = Variable ProdVarName | Value Val deriving Show







----------------------------------------------------------------------------------------------------------------
-- Logic & Functions
----------------------------------------------------------------------------------------------------------------

filterRule :: (Ord t) => Rule t -> Either String ProductionRule
filterRule x
    | isRule x = Right $ ruleToProductionRule x
    | otherwise = Left $ "Not a valid rule: " ++ arNameToString (nameOfRule x) ++ "\n"

ruleToProductionRule :: (Ord t) => Rule t -> ProductionRule
ruleToProductionRule Rule {nameOfRule, varDeclsOfRule, precondOfRule, postcondOfRule}
    = ProductionRule { nameOfProductionRule = arNameToString nameOfRule
                     , varDeclsOfProductionRule = [""]
                     , leftHandSide = exprlistToRCList S.empty $ precondToRCList precondOfRule -- precondToRuleCondition precondOfRule
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

precondToRCList :: Expr t -> [Expr t] -- todo : rename to reflect new typesig
precondToRCList (BinOpE _ (BBool BBand) arg1 arg2) = precondToRCList arg1 ++ precondToRCList arg2
precondToRCList fApp@(AppE {}) = [fApp]
precondToRCList bcomp@(BinOpE _ (BCompar _) _ _) = [bcomp]
precondToRCList _ = error "non and operation"

exprlistToRCList :: (Ord t) => S.Set (Var t) -> [Expr t] -> RCList
exprlistToRCList vs (x@(AppE {}):xs) =
    let appVars = localVariables x
    in exprToConditionalFuncApp x : exprlistToRCList (S.union appVars vs) xs
exprlistToRCList vs (x@(BinOpE _ (BCompar _) _ _): xs) =
    let compVars = localVariables x
    in if S.isSubsetOf compVars vs then exprToConditionalEval x : exprlistToRCList (S.union compVars vs) xs else error "Reorder ur predicates"
exprlistToRCList _ [] = []
exprlistToRCList _ _ = error "exprToRCList used with non-function application or comparison operation"

localVariables :: (Ord t) => Expr t -> S.Set (Var t)
localVariables (AppE _ f a) = S.union (localVariables f) (localVariables a)
localVariables (BinOpE _ _ a1 a2) = S.union (localVariables a1) (localVariables a2)
localVariables (VarE _ val) = case val of
  GlobalVar _ -> S.empty
  LocalVar _ _ -> S.singleton val
localVariables _ = error "localVariables used with non-function application or comparison operation"

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


astToRules :: Program (Tp ()) -> IO ()
astToRules x = do
    let lrRules = map filterRule $ rulesOfProgram x
        gdRules = rights lrRules
    -- print $ lefts lrRules
    -- print $ gdRules !! 4
    putStrLn ""
    -- putStrLn "Clara:"
    putDoc $ showClara (gdRules !! 4)
    putStrLn ""
    -- putStrLn "Drools:"
    -- putDoc $ showDrools (gdRules !! 4)
