module ExpSysTest where

import Syntax
import Annotation
import MainHelpers ( getTpAst )

import Control.Monad.Except (runExceptT)
import qualified SimpleRules as SR
import qualified ToRules.FromL4 as TRL4
import qualified ToRules.Types as TRTp
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Data.Either ( rights )
import Data.Set as S
import SyntaxManipulation (appToFunArgs)



-- given a filepath, return a program
getProg :: FilePath -> IO (Program (Tp ()))
getProg fpath = do
    contents <- readFile fpath
    errOrTpAst <- runExceptT $ getTpAst fpath contents
    case errOrTpAst of
        Right prog -> return $ typeAnnot <$> prog
        Left errs -> do
            error $ show errs -- TODO: Add error printing with proper formatting

-- given rulename, filter out rule from program
obtRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
obtRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]




esGraphUTs :: TestTree
esGraphUTs = withResource acquire release $ \progIO->
    testGroup "Expert System: Graph Generation Tests"
        [ testGroup "isRule"
            [ testCase "returns True for <accInad>" $ do
                rule <- progToRule progIO "accInad"
                SR.isRule rule @?= True
            , testCase "returns True for <accAdIncInad>" $ do
                rule <- progToRule progIO "accAdIncInad"
                SR.isRule rule @?= True
            , testCase "returns True for <savingsAd>" $ do
                rule <- progToRule progIO "savingsAd"
                SR.isRule rule @?= True
            ]
        , testGroup "flattenConjs"
            [ testCase "returns 1 AppE for preconds of <accInad>" $ do
                rule <- progToRule progIO "accInad"
                length (SR.flattenConjs . precondOfRule $ rule) @?= 1
            , testCase "returns 2 AppE for preconds of <accAdIncInad>" $ do
                rule <- progToRule progIO "accAdIncInad"
                length (SR.flattenConjs . precondOfRule $ rule) @?= 2
            , testCase "returns 2 AppE for preconds of <savingsAd>" $ do
                rule <- progToRule progIO "savingsAd"
                length (SR.flattenConjs . precondOfRule $ rule) @?= 2
            ]
        , testCase "print the set of nodes and edges for a single rule" $ do
                rule <- progToRule progIO "accInad"
                print $ SR.simpleRulesToGrNodes $ rights [SR.ruleToSimpleRule rule]
                'a' @?= 'a'
        , testCase "mapping a single rule twice = convert a list of simple rules" $ do
            rule1 <- progToRule progIO "accInad"
            rule2 <- progToRule progIO "savingsAd"
            let (nodes1, edges1) = SR.simpleRuleToGrNodes $ head . rights $ [SR.ruleToSimpleRule rule1]
                (nodes2, edges2) = SR.simpleRuleToGrNodes $ head . rights $ [SR.ruleToSimpleRule rule2]
            SR.simpleRulesToGrNodes (rights $ Prelude.map SR.ruleToSimpleRule [rule1, rule2]) @?= (S.union nodes1 nodes2, S.union edges1 edges2)
        ]
    where acquire = getProg "tests/ExpSysTestFiles/financial_advisor.l4"
          release = mempty
          progToRule progIO rname = do
              prog <- progIO
              pure $ head $ obtRule prog rname


esRuleUTs :: TestTree
esRuleUTs = withResource acquire release $ \progIO->
    testGroup "Expert System: Rule Translation Tests" -- todo: fill in the rest
        [ testGroup "Type: CEArg"
            [ testGroup "exprToCEBindEq"
                [ testCase "returns CEBinding for a LocalVar expr" $ do
                    rule <- progToRule progIO "singleLocalVar"
                    let (_, varExpr) = appToFunArgs [] $ precondOfRule rule
                    TRL4.exprToCEBindEq (0, head varExpr) @?= TRTp.CEBinding "x" "arg0"
                , testCase "returns CEEquality for a GlobalVar expr" $ do
                    rule <- progToRule progIO "singleGlobalVar"
                    let (_, varExpr) = appToFunArgs [] $ precondOfRule rule
                    TRL4.exprToCEBindEq (0, head varExpr) @?= TRTp.CEEquality "arg0" "inadequate"
                ]
            , testGroup "exprToCEFuncApp"
                [ testCase "returns CEFuncApp for a AppE" $ do
                    rule <- progToRule progIO "singleGlobalVar"
                    TRL4.exprToCEFuncApp (precondOfRule rule) @?= TRTp.CEFuncApp "savings_account" ["inadequate"]
                ]
            ]
        , testGroup "Type: ConditionalElement" 
            [ testGroup "exprToConditionalFuncApp"
                [ testCase "returns ConditionalFuncApp for an AppE" $ do
                    rule <- progToRule progIO "singleGlobalVar"
                    TRL4.exprToConditionalFuncApp (precondOfRule rule) @?= TRTp.ConditionalFuncApp "savings_account" [TRTp.CEEquality "arg0" "inadequate"]
                ]
            , testGroup "exprToConditionalEval"
                [ testCase "returns ConditionalEval for BinOpE with BClt" $ do
                    rule <- progToRule progIO "preCond_singleEval_BClt"
                    let evalExpr = last . TRL4.precondToExprList . precondOfRule $ rule
                    TRL4.exprToConditionalEval evalExpr @?= TRTp.ConditionalEval BClt (TRTp.CEVarExpr "x") (TRTp.CEVarExpr "y")
                ]
            , testGroup "exprToConditionalExist"
                [ testCase "returns ConditionalExist for UnaOpE with UBnot" $ do
                    rule <- progToRule progIO "preCond_singleNot_withBinding"
                    let notExpr = last . TRL4.precondToExprList . precondOfRule $ rule
                    TRL4.exprToConditionalExist notExpr @?= TRTp.ConditionalExist UBnot (TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"])
                ]
            ]
        , testGroup "Function: exprlistToRCList" 
            [ testCase "returns ConditionalFuncApp for an AppE" $ do
                rule <- progToRule progIO "singleGlobalVar"
                TRL4.exprlistToRCList S.empty [precondOfRule rule] @?= [TRTp.ConditionalFuncApp "savings_account" [TRTp.CEEquality "arg0" "inadequate"]]
            , testCase "returns [ConditionalFuncApp, ConditionalFuncApp, ConditionalEval] for preCond_singleEval_BClt" $ do
                rule <- progToRule progIO "preCond_singleEval_BClt"
                TRL4.exprlistToRCList  S.empty (TRL4.precondToExprList $ precondOfRule rule) @?= [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"], TRTp.ConditionalFuncApp "dependents" [TRTp.CEBinding "y" "arg0"], TRTp.ConditionalEval BClt (TRTp.CEVarExpr "x") (TRTp.CEVarExpr "y")]
            , testCase "returns [ConditionalFuncApp, ConditionalExist] for preCond_singleNot_withBinding" $ do
                rule <- progToRule progIO "preCond_singleNot_withBinding"
                TRL4.exprlistToRCList S.empty (TRL4.precondToExprList . precondOfRule $ rule) @?= [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"], TRTp.ConditionalExist UBnot (TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"])]
            , testCase "fails for preCond_singleNot_noBinding" $ do
                rule <- progToRule progIO "preCond_singleNot_noBinding"
                TRL4.exprlistToRCList S.empty [precondOfRule rule] @?= [TRTp.ConditionalElementFail "`Not` statements require a prior variable binding"]
            , testCase "fails for preCond_singleEval_noBinding" $ do
                rule <- progToRule progIO "preCond_singleEval_noBinding"
                TRL4.exprlistToRCList S.empty (TRL4.precondToExprList $ precondOfRule rule) @?= [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"], TRTp.ConditionalElementFail "Reorder ur predicates"]
            , testCase "returns [ConditionalFuncApp, ConditionalFuncApp, ConditionEval (CEArithmetic) (CELiteral)] for preCond_arith_2args" $ do
                rule <- progToRule progIO "preCond_arith_2args"
                TRL4.exprlistToRCList S.empty (TRL4.precondToExprList . precondOfRule $ rule) @?= [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"],TRTp.ConditionalFuncApp "dependents" [TRTp.CEBinding "y" "arg0"],TRTp.ConditionalEval BCgt (TRTp.CEArithmetic BAadd (TRTp.CEVarExpr "x") (TRTp.CEVarExpr "y")) (TRTp.CELiteral "10")]
            , testCase "returns [ConditionalFuncApp, ConditionalFuncApp, ConditionEval (CEArithmetic (CEArithmetic)) (CELiteral)] for preCond_arith_3args" $ do
                rule <- progToRule progIO "preCond_arith_3args"
                TRL4.exprlistToRCList S.empty (TRL4.precondToExprList . precondOfRule $ rule) @?= [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"],TRTp.ConditionalFuncApp "dependents" [TRTp.CEBinding "y" "arg0"],TRTp.ConditionalFuncApp "earnings" [TRTp.CEBinding "z" "arg0",TRTp.CEEquality "arg1" "steady"],TRTp.ConditionalEval BCgt (TRTp.CEArithmetic BAsub (TRTp.CEArithmetic BAadd (TRTp.CEVarExpr "x") (TRTp.CEVarExpr "y")) (TRTp.CEVarExpr "z")) (TRTp.CELiteral "10")] 
            , testCase "fails for preCond_arith_noBinding" $ do
                rule <- progToRule progIO "preCond_arith_noBinding"
                TRL4.exprlistToRCList S.empty (TRL4.precondToExprList .precondOfRule $ rule) @?= [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg0"],TRTp.ConditionalElementFail "Reorder ur predicates"]
            ]
        , testGroup "Type: RuleAction" 
            [ testGroup "exprToRuleAction" 
                [ expectFail $ testCase "returns ActionExprErr for AppE with GlobalVar" $ do
                    rule <- progToRule progIO "postCond_withGlobal"
                    TRL4.exprToRuleAction (postcondOfRule rule) @?= (TRTp.ActionFuncApp "investment" [TRTp.Value "savings"])
                , expectFailBecause "not implemented yet" $ testCase "returns ActionExprErr for AppE with LocalVar with prior binding in preconds" $ do
                    rule <- progToRule progIO "postCond_withLocal_withBinding"
                    1 @?= 0
                , expectFailBecause "not implemented yet" $ testCase "fails for postCond_withLocal_withoutBinding" $ do
                    rule <- progToRule progIO "postCond_withLocal_withoutBinding"
                    1 @?= 0
                ]

            ]
        ]
    where acquire = getProg "tests/ExpSysTestFiles/finAd_tests.l4"
          release = mempty
          progToRule progIO rname = do
              prog <- progIO
              pure $ head $ obtRule prog rname