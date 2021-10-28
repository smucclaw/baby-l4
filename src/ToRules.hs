{-# LANGUAGE NamedFieldPuns #-}
module ToRules where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import L4.Syntax
import SyntaxManipulation (appToFunArgs)
import L4LSP (arNameToString)
import SimpleRules (isRule)
import Data.Either (rights)
import Data.Char (toUpper)
import qualified Data.Set as S
import Util (capitalise)
import SyntaxManipulation (appToFunArgs)

data RuleFormat = Clara | Drools deriving Eq

class ShowClara x where
    showClara :: x -> Doc ann

class ShowDrools x where
    showDrools :: x -> Doc ann

-- Some notes:

-- Currently we have failure types for ConditionalElement & CEArg to allow for tests
-- of expected failure (especially so for testing if vars are bound in rules). It's
-- probably not wise to extend this treatment to the other types, and we should begin 
-- incorporating ExceptT (& other necessary monad transformers) for error-handling. 



----------------------------------------------------------------------------------------------------------------
-- Types & Instances
----------------------------------------------------------------------------------------------------------------

type Typename = String
type ProdVarName = String
type ProdFieldName = String
type ProdFuncName = String

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
                                               }
data ProductionClassField = ProductionClassField ProdFieldName ProductionClassType
newtype ProductionClassType = ProductionClassType Typename


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
    | ConditionalExist UBoolOp ConditionalElement 
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
           | CELiteral ProdVarName 
           | CEVarExpr ProdVarName 
           | CEArgFail String
           deriving (Eq, Show)

instance ShowClara CEArg where
    showClara (CEEquality fn fv) = pretty "=" <+> pretty fn <+> pretty fv
    showClara (CEBinding vn fn) = pretty "=" <+> pretty "?" <> pretty vn <+> pretty fn
    showClara (CEArithmetic aOp v1 v2) = lparen <> showClara aOp <+> showClara v1 <+> showClara v2 <> rparen
    showClara (CEFuncApp func vns) = parens (pretty func <+> hsep (punctuate comma (map ((<>) (pretty "?") . pretty) vns)))
    showClara (CELiteral x) = pretty x
    showClara (CEVarExpr vn) = pretty "?" <> pretty vn
    showClara (CEArgFail err) = error $ "Transpilation failure: " ++ show err 

instance ShowDrools CEArg where
    showDrools (CEEquality fn fv) = pretty fn <+> pretty "==" <+> pretty fv
    showDrools (CEBinding vn fn) = pretty "$" <> pretty vn <+> pretty ":=" <+> pretty fn
    showDrools (CEArithmetic aOp v1 v2) = lparen <> showDrools v1 <+> showDrools aOp <+> showDrools v2 <> rparen
    showDrools (CEFuncApp func vns) = pretty func <> parens (hsep (punctuate comma (map ((<>) (pretty "$") . pretty) vns)))
    showDrools (CELiteral x) = pretty x
    showDrools (CEVarExpr vn) = pretty "$" <> pretty vn
    showDrools (CEArgFail err) = error $ "Transpilation failure: " ++ show err 

-- We restrict the format of the rules to a singular post condition (to support prolog-style syntax within l4)
data RuleAction = ActionFuncApp Typename [Argument] 
                | ActionExprErr String
                deriving (Eq, Show)

instance ShowClara RuleAction where
    showClara (ActionFuncApp fname args) = pretty "(c/insert! (->" <> pretty fname <+> hsep (map ((<>) (pretty ":") . showClara) args)
    showClara (ActionExprErr x) = pretty x

instance ShowDrools RuleAction where
    showDrools (ActionFuncApp fname args) = pretty "insertLogical(new" <+> pretty fname <> parens ( hsep (punctuate comma $ map (squotes . showDrools) args)) <> pretty ");"
    showDrools (ActionExprErr x) = pretty x


data Argument = Variable ProdVarName | Value String deriving (Eq, Show)

instance ShowClara Argument where
    showClara (Variable x) = pretty x
    showClara (Value y)    = pretty y

instance ShowDrools Argument where
    showDrools (Variable x) = pretty x
    showDrools (Value y)    = pretty y







----------------------------------------------------------------------------------------------------------------
-- Logic & Functions
----------------------------------------------------------------------------------------------------------------

filterRule :: (Ord t, Show t) => Rule t -> Either String ProductionRule
filterRule x
    | isRule x = Right $ ruleToProductionRule x
    | otherwise = Left $ "Not a valid rule: " ++ arNameToString (nameOfRule x) ++ "\n"

ruleToProductionRule :: (Ord t, Show t) => Rule t -> ProductionRule
ruleToProductionRule Rule {nameOfRule, varDeclsOfRule, precondOfRule, postcondOfRule}
    = ProductionRule { nameOfProductionRule = arNameToString nameOfRule
                     , varDeclsOfProductionRule = [""]
                     , leftHandSide = exprlistToRCList S.empty $ precondToExprList precondOfRule -- precondToRuleCondition precondOfRule
                     , rightHandSide = [exprToRuleAction postcondOfRule]
                     }

varDeclToProdVarName :: VarDecl t -> ProdVarName
varDeclToProdVarName = undefined

capitalize :: String -> String
capitalize xs = toUpper (head xs) : tail xs

precondToExprList :: Expr t -> [Expr t] -- todo : rename to reflect new typesig
precondToExprList (BinOpE _ (BBool BBand) arg1 arg2) = precondToExprList arg1 ++ precondToExprList arg2
precondToExprList fApp@(AppE {}) = [fApp]
precondToExprList bcomp@(BinOpE _ (BCompar _) _ _) = [bcomp]
precondToExprList unot@(UnaOpE _ (UBool UBnot) _) = [unot]
precondToExprList _ = error "non and operation"

exprlistToRCList :: (Ord t, Show t) => S.Set (Var t) -> [Expr t] -> RCList
exprlistToRCList vs (x:xs) = 
    case x of 
        (AppE {})                   -> exprToConditionalFuncApp x : exprlistToRCList newVars xs
        (BinOpE _ (BCompar _) _ _)  -> if S.isSubsetOf xlocVars vs then exprToConditionalEval x : exprlistToRCList newVars xs else [ConditionalElementFail "Reorder ur predicates"]
        (UnaOpE _ (UBool UBnot) _)  -> if S.isSubsetOf xlocVars vs then exprToConditionalExist x : exprlistToRCList newVars xs else [ConditionalElementFail "`Not` statements require a prior variable binding"]
        _                           -> [ConditionalElementFail "exprToRCList can only be used with function application, comparison operation or negation"]
    where xlocVars = localVariables x
          newVars = S.union xlocVars vs
exprlistToRCList _ [] = []

localVariables :: (Ord t) => Expr t -> S.Set (Var t)
localVariables (AppE _ f a) = S.union (localVariables f) (localVariables a)
localVariables (BinOpE _ _ a1 a2) = S.union (localVariables a1) (localVariables a2)
localVariables (UnaOpE _ _ a) = localVariables a
localVariables (ValE _ _) = S.empty
localVariables (VarE _ val) = case val of
  GlobalVar _ -> S.empty
  LocalVar _ _ -> S.singleton val
localVariables _ = error "localVariables used with non-function application or comparison operation"

getName :: Expr t -> VarName
getName = nameOfQVarName . nameOfVar . varOfExprVarE

exprToConditionalFuncApp :: (Show t) => Expr t -> ConditionalElement
exprToConditionalFuncApp fApp@AppE {} =
    let (fexpr, args) = appToFunArgs [] fApp
    in ConditionalFuncApp (getName fexpr) (map (exprToCEBindEq) $ zip [0.. ((length args)-1)] args)
exprToConditionalFuncApp _ = error "exprToConditionalFuncApp used for non-AppE"

exprToConditionalEval :: (Show t) => Expr t -> ConditionalElement
exprToConditionalEval (BinOpE _ (BCompar bop) x@AppE {} y@AppE {}  ) = ConditionalEval bop (exprToCEFuncApp x) (exprToCEFuncApp y)
exprToConditionalEval (BinOpE _ (BCompar bop) x@AppE {} y          ) = ConditionalEval bop (exprToCEFuncApp x) (exprToCEArg y)
exprToConditionalEval (BinOpE _ (BCompar bop) x           y@AppE {}) = ConditionalEval bop (exprToCEArg x) (exprToCEFuncApp y)
exprToConditionalEval (BinOpE _ (BCompar bop) x           y        ) = ConditionalEval bop (exprToCEArg x) (exprToCEArg y)
exprToConditionalEval _ = error "exprToConditionalEval used for non-BComparOp"

exprToConditionalExist :: (Show t) => Expr t -> ConditionalElement -- we restrict not to function applications with variable bindings
exprToConditionalExist (UnaOpE _ (UBool UBnot) a@AppE {}) = ConditionalExist UBnot $ exprToConditionalFuncApp a
exprToConditionalExist _ = error "exprToConditionalExist used for non-UnaOpE"

defArg :: Int -> ProdFieldName
defArg x = "arg" ++ show x

-- a note: CEFuncApp is meant to be part of the ConditionalEval expression (e.g. the `minIncome y` of `x > minIncome y`)
--         rather than a ConditionalFuncApp expression (e.g. `amount_saved x`)
--         but they both act on AppE expressions, so there is no need to distinguish between testing mechanisms
exprToCEFuncApp :: (Show t) => Expr t -> CEArg
exprToCEFuncApp fApp@AppE {} =
    let (fexpr, args) = appToFunArgs [] fApp
    in CEFuncApp (getName fexpr) (map getName args)
exprToCEFuncApp expr = CEArgFail $ "exprToCEFuncApp received non-AppE expr" ++ show expr

exprToCEBindEq :: (Show t) => (Int, Expr t) -> CEArg -- assumption: either local or global var expr
exprToCEBindEq (num, VarE _ (LocalVar name _)) = CEBinding (nameOfQVarName name) (defArg num)
exprToCEBindEq (num, VarE _ (GlobalVar name)) = CEEquality (defArg num) (nameOfQVarName name)
exprToCEBindEq (_, expr) = CEArgFail $ "exprToCEBindEq cannot transpile expression: " ++ show expr

exprToCEArg :: (Show t) => Expr t -> CEArg
exprToCEArg (BinOpE _ (BArith aOp) x y) = CEArithmetic aOp (exprToCEArg x) (exprToCEArg y)
exprToCEArg (ValE _ x) = CELiteral (case x of
   BoolV b -> show b
   IntV n -> show n
   FloatV y -> show y
   StringV s -> show s
   ErrV -> error "The compiler should have failed before transpilation occurs here.") 
exprToCEArg ve@(VarE _ _) = CEVarExpr $ getName ve
exprToCEArg _ = CEArgFail "you shouldn't have gotten this"

-- TODO: Account for local variable bindings within post condition fApps
exprToRuleAction :: (Show t) => Expr t -> RuleAction
exprToRuleAction fApp@(AppE {}) = 
    let (fexpr, args) = appToFunArgs [] fApp 
    in ActionFuncApp (getName fexpr) (map (Value . getName) args)
exprToRuleAction x = ActionExprErr $ "error: cannot convert expression into rule-action: " ++ show x  

obtRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
obtRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]

astToRules :: Program (Tp ()) -> IO ()
astToRules x = do
    let lrRules = map filterRule $ rulesOfProgram x
        gdRules = rights lrRules
        r1 = map filterRule $ obtRule x "preCond_arith_2args"
        r2 = map filterRule $ obtRule x "preCond_arith_3args"
        rf = map filterRule $ obtRule x "preCond_arith_noBinding"
        rules = rights $ r1 ++ r2 ++ rf 
    -- print $ lefts lrRules
    putStrLn "Rule AST:"
    -- print $ gdRules !! 0
    -- print $ gdRules !! 4
    putStrLn ""
    putStrLn "Clara:"
    putDoc $ showClara $ rules !! 0
    putStrLn ""
    putStrLn "Drools:"
    putDoc $ showDrools $ rules !! 0
    -- putDoc $ showDrools (gdRules !! 4)
    putStrLn ""
    putStrLn "Clara:"
    putDoc $ showClara $ rules !! 1
    putStrLn ""
    putStrLn "Drools:"
    putDoc $ showDrools $ rules !! 1
    putStrLn ""
    putStrLn "Clara:"
    putDoc $ showClara $ rules !! 2
    putStrLn ""
    putStrLn "Drools:"
    putDoc $ showDrools $ rules !! 2
