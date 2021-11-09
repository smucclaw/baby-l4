{-# LANGUAGE NamedFieldPuns #-}
module ToRules.ToRules where

import ToRules.Types
import Prettyprinter (line)
import L4.Syntax
import L4.SyntaxManipulation (appToFunArgs)
import L4LSP (arNameToString)
import SimpleRules (isRule)
import Data.Either (rights)
import qualified Data.Set as S
import Util (capitalise)
import Data.Maybe (mapMaybe)

-- TODO 22/10/2021
--  1) account for bindings within post conditions
--      25/10 : binding outputs for post conditions are done.
--      29/10: added checks for existence of bindings for local variables used in rule actions
--  2) generate justification objects on a per-rule basis
--      27/10 : justification objects are a go go go
--      29/10 : justification fields added to type declarations
--      Next: tests?
--  3) integrate justification objects into rule transpilations
--      a) positive literals should have bindings to their justification objects
--      b) `not` expressions (negative literals) should not have bindings to their justifications (since bindings don't make sense within a `not` statement for drools)
--      c) evaluative expressions should not have bindings to their justifications 
--  
-- Thoughts 22/10/2021
--  1) how is a single rule containing a `not` expression with equality test currently transcribed?
--       >  with the `:=` syntax in drools being a binding in first occurrence and equality after,
--          it is likely that the `not` expression would not have the expected interpretation in drools.




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
    = ProductionRule { nameOfProductionRule = prodRuleName
                     , varDeclsOfProductionRule = [""]
                     , leftHandSide =  condElems -- exprlistToRCList S.empty $ precondToExprList precondOfRule 
                     , rightHandSide = [exprToRuleAction boundLocals traceObj postcondOfRule]
                     , traceObj = traceObj
                     }
    where prodRuleName = arNameToString nameOfRule
          (boundLocals, condElems) = exprlistToRCList 0 S.empty [] $  precondToExprList precondOfRule
          traceObj = mkTrace prodRuleName condElems varDeclsOfRule

mkTrace :: String -> [ConditionalElement] -> [VarDecl t] -> ProductionRuleTrace
mkTrace rnm priors args = ProductionRuleTrace {
      nameOfTraceObj = "justification" ++ capitalise rnm
    , priors = mapMaybe condElemToTraceTup priors
    , args = mapMaybe varDeclToTraceTup args
}

condElemToTraceTup :: ConditionalElement -> Maybe TraceTuple
condElemToTraceTup (ConditionalFuncApp s _) = Just $ TraceTup (s, "Justification")
condElemToTraceTup _ = Nothing

varDeclToTraceTup :: VarDecl t -> Maybe TraceTuple
varDeclToTraceTup (VarDecl _ nm (ClassT _ (ClsNm cn))) = Just $ TraceTup (nm, cn)
varDeclToTraceTup _ = Nothing

precondToExprList :: Expr t -> [Expr t] -- todo : rename to reflect new typesig
precondToExprList (BinOpE _ (BBool BBand) arg1 arg2) = precondToExprList arg1 ++ precondToExprList arg2
precondToExprList fApp@AppE {} = [fApp]
precondToExprList bcomp@(BinOpE _ (BCompar _) _ _) = [bcomp]
precondToExprList unot@(UnaOpE _ (UBool UBnot) _) = [unot]
precondToExprList _ = error "non and operation"

exprlistToRCList :: (Ord t, Show t) => Int -> S.Set (Var t) -> RCList -> [Expr t] -> (S.Set (Var t), RCList)
exprlistToRCList num vs acc (x:xs) = exprlistToRCList (num + 1) nvs (acc <> [ce]) xs where (nvs, ce) = exprToRC num (vs, x)
exprlistToRCList _ vs acc [] = (vs, acc)

exprToRC :: (Ord t, Show t) => Int -> (S.Set (Var t), Expr t) -> (S.Set (Var t), ConditionalElement)
exprToRC num (vs, x) =
    case x of
        AppE {}                   -> (newVars, exprToConditionalFuncApp num x)
        (BinOpE _ (BCompar _) _ _)  -> if S.isSubsetOf xlocVars vs then (newVars, exprToConditionalEval x)  else (vs, ConditionalElementFail "Reorder ur predicates")
        (UnaOpE _ (UBool UBnot) _)  -> if S.isSubsetOf xlocVars vs then (newVars, exprToConditionalExist x)  else (vs, ConditionalElementFail "`Not` statements require a prior variable binding")
        _                           -> (vs, ConditionalElementFail "exprToRCList can only be used with function application, comparison operation or negation")
    where xlocVars = localVariables x
          newVars = S.union xlocVars vs

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

-- Perhaps we should define a Justification type instead?
exprToConditionalFuncApp :: (Show t) => Int -> Expr t -> ConditionalElement
exprToConditionalFuncApp num fApp@AppE {} = ConditionalFuncApp (getName fexpr) faArgs
    where (fexpr, args) = appToFunArgs [] fApp
          justBind = CEBinding ("j" ++ show num) "arg0"
          remArgs = map (exprToCEBindEq) (zip [1.. (length args)] args)
          faArgs = if num < 0 then remArgs else justBind : remArgs
exprToConditionalFuncApp _ _ = error "exprToConditionalFuncApp used for non-AppE"

exprToConditionalEval :: (Show t) => Expr t -> ConditionalElement
exprToConditionalEval (BinOpE _ (BCompar bop) x@AppE {} y@AppE {}  ) = ConditionalEval bop (exprToCEFuncApp x) (exprToCEFuncApp y)
exprToConditionalEval (BinOpE _ (BCompar bop) x@AppE {} y          ) = ConditionalEval bop (exprToCEFuncApp x) (exprToCEArg y)
exprToConditionalEval (BinOpE _ (BCompar bop) x           y@AppE {}) = ConditionalEval bop (exprToCEArg x) (exprToCEFuncApp y)
exprToConditionalEval (BinOpE _ (BCompar bop) x           y        ) = ConditionalEval bop (exprToCEArg x) (exprToCEArg y)
exprToConditionalEval _ = error "exprToConditionalEval used for non-BComparOp"

exprToConditionalExist :: (Show t) => Expr t -> ConditionalElement -- we restrict `not` to function applications with variable bindings
exprToConditionalExist (UnaOpE _ (UBool UBnot) a@AppE {}) = ConditionalExist UBnot $ exprToConditionalFuncApp (-1) a
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
exprToCEArg (ValE _ ErrV) = error "The compiler should have failed before transpilation occurs here."
exprToCEArg (ValE _ x) = CELiteral x
exprToCEArg ve@(VarE _ (GlobalVar {})) = CELiteral $ StringV $ getName ve
exprToCEArg ve@(VarE _ (LocalVar {})) = CEVarExpr $ LocVar $ getName ve
exprToCEArg _ = CEArgFail "you shouldn't have gotten this"

exprToRuleAction :: (Ord t, Show t) => S.Set (Var t) -> ProductionRuleTrace -> Expr t -> RuleAction
exprToRuleAction lvs tObj fApp@(AppE {}) = ActionFuncApp (getName fexpr) tObj (map (checkBoundLocal lvs) args)
    where (fexpr, args) = appToFunArgs [] fApp
          checkBoundLocal bls x@(VarE _ c@(LocalVar{})) = if S.member c bls then exprToCEArg x else error $ "Local variable defined in rule action not previously bound: " ++ show x
          checkBoundLocal _ x = exprToCEArg x

exprToRuleAction _ _ x = ActionExprErr $ "error: cannot convert expression into rule-action: " ++ show x

astToRules :: RuleFormat -> Program (Tp ()) -> IO ()
astToRules rf x = do
    let lrRules = map filterRule $ rulesOfProgram x
        gdRules = rights lrRules
    mapM_ (print . (<>) line . showForm rf) gdRules
