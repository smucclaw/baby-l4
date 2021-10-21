{-# LANGUAGE NamedFieldPuns #-}
module ToRules.FromL4 where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import ToRules.Types
import ToRules.ToDecls (astToDecls)
import Syntax
import SyntaxManipulation (appToFunArgs)
import L4LSP (arNameToString)
import SimpleRules (isRule)
import Data.Either (rights)
import qualified Data.Set as S
import PPC.Instr (Instr(XORIS))
import Util (capitalise)

data RuleFormat = Clara | Drools deriving Eq

-- Some notes:

-- Currently we have failure types for ConditionalElement & CEArg to allow for tests
-- of expected failure (especially so for testing if vars are bound in rules). It's
-- probably not wise to extend this treatment to the other types, and we should begin 
-- incorporating ExceptT (& other necessary monad transformers) for error-handling. 


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

precondToExprList :: Expr t -> [Expr t] -- todo : rename to reflect new typesig
precondToExprList (BinOpE _ (BBool BBand) arg1 arg2) = precondToExprList arg1 ++ precondToExprList arg2
precondToExprList fApp@AppE {} = [fApp]
precondToExprList bcomp@(BinOpE _ (BCompar _) _ _) = [bcomp]
precondToExprList unot@(UnaOpE _ (UBool UBnot) _) = [unot]
precondToExprList _ = error "non and operation"

exprlistToRCList :: (Ord t, Show t) => S.Set (Var t) -> [Expr t] -> RCList
exprlistToRCList vs (x:xs) =
    case x of
        AppE {}                   -> exprToConditionalFuncApp x : exprlistToRCList newVars xs
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
    mapM_ (print . (<>) line . showDrools) gdRules


genREP :: Program (Tp ()) -> IO ()
genREP x = do
    astToDecls x 
    astToRules x