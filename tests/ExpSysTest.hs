module ExpSysTest where

import L4.Syntax
import L4.Annotation
import MainHelpers ( getTpAst )

import Control.Monad.Except (runExceptT)
import qualified SimpleRules as SR
import qualified ToRules.ToRules as TRTR
import qualified ToRules.Types as TRTp
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Data.Either ( rights )
import Data.Set as S
import Data.Tuple (snd)
import L4.SyntaxManipulation (appToFunArgs)



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

-- extract a single rule with a given name
progToRule :: IO (Program (Tp ())) -> String -> IO (Rule (Tp ()))
progToRule progIO rname = do
    prog <- progIO
    pure $ head $ obtRule prog rname

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


esRuleUTs :: TestTree
esRuleUTs = withResource acquire release $ \progIO->
    testGroup "Expert System: Rule Translation Tests" -- todo: fill in the rest
        [ testGroup "Type: CEArg"
            [ testGroup "exprToCEBindEq"
                [ testCase "returns CEBinding for a LocalVar expr" $ do
                    rule <- progToRule progIO "singleLocalVar"
                    let (_, varExpr) = appToFunArgs [] $ precondOfRule rule
                    TRTR.exprToCEBindEq (0, head varExpr) @?= TRTp.CEBinding "x" "arg0"
                , testCase "returns CEEquality for a GlobalVar expr" $ do
                    rule <- progToRule progIO "singleGlobalVar"
                    let (_, varExpr) = appToFunArgs [] $ precondOfRule rule
                    TRTR.exprToCEBindEq (0, head varExpr) @?= TRTp.CEEquality "arg0" "inadequate"
                ]
            , testGroup "exprToCEFuncApp"
                [ testCase "returns CEFuncApp for a AppE" $ do
                    rule <- progToRule progIO "singleGlobalVar"
                    TRTR.exprToCEFuncApp (precondOfRule rule) @?= TRTp.CEFuncApp "savings_account" ["inadequate"]
                ]
            ]
        , testGroup "Type: ConditionalElement" 
            [ testGroup "exprToConditionalFuncApp"
                [ testCase "returns ConditionalFuncApp for an AppE" $ do
                    rule <- progToRule progIO "singleGlobalVar"
                    TRTR.exprToConditionalFuncApp 0 (precondOfRule rule) @?= TRTp.ConditionalFuncApp "savings_account" [TRTp.CEBinding "j0" "arg0",TRTp.CEEquality "arg1" "inadequate"]
                ]
            , testGroup "exprToConditionalEval"
                [ testCase "returns ConditionalEval for BinOpE with BClt" $ do
                    rule <- progToRule progIO "preCond_singleEval_BClt"
                    let evalExpr = last . TRTR.precondToExprList . precondOfRule $ rule
                    TRTR.exprToConditionalEval evalExpr @?= TRTp.ConditionalEval BClt (TRTp.CEVarExpr . TRTp.LocVar $"x") (TRTp.CEVarExpr . TRTp.LocVar $"y")
                ]
            , testGroup "exprToConditionalExist"
                [ testCase "returns ConditionalExist for UnaOpE with UBnot" $ do
                    rule <- progToRule progIO "preCond_singleNot_withBinding"
                    let notExpr = last . TRTR.precondToExprList . precondOfRule $ rule
                    TRTR.exprToConditionalExist notExpr @?= TRTp.ConditionalExist UBnot (TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg1"])
                ]
            ]
        , testGroup "Function: exprlistToRCList" 
            [ testCase "returns ConditionalFuncApp for an AppE" $ 
                test_ExprListToRCList progIO "singleGlobalVar"                  [TRTp.ConditionalFuncApp "savings_account" [TRTp.CEBinding "j0" "arg0",TRTp.CEEquality "arg1" "inadequate"]]

            , testCase "returns [TRTp.TRTp.ConditionalFuncApp, TRTp.ConditionalFuncApp, TRTp.ConditionalEval] for preCond_singleEval_BClt" $ 
                test_ExprListToRCList progIO "preCond_singleEval_BClt"          [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "j0" "arg0",TRTp.CEBinding "x" "arg1"],TRTp.ConditionalFuncApp "dependents" [TRTp.CEBinding "j1" "arg0",TRTp.CEBinding "y" "arg1"],TRTp.ConditionalEval BClt (TRTp.CEVarExpr . TRTp.LocVar $"x") (TRTp.CEVarExpr . TRTp.LocVar $"y")]

            , testCase "returns [TRTp.TRTp.ConditionalFuncApp, TRTp.ConditionalExist] for preCond_singleNot_withBinding" $ 
                test_ExprListToRCList progIO "preCond_singleNot_withBinding"    [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "j0" "arg0",TRTp.CEBinding "x" "arg1"],TRTp.ConditionalExist UBnot (TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "x" "arg1"])]

            , testCase "fails for preCond_singleNot_noBinding" $ 
                test_ExprListToRCList progIO "preCond_singleNot_noBinding"      [TRTp.ConditionalElementFail "`Not` statements require a prior variable binding"]

            , testCase "fails for preCond_singleEval_noBinding" $
                test_ExprListToRCList progIO "preCond_singleEval_noBinding"     [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "j0" "arg0",TRTp.CEBinding "x" "arg1"],TRTp.ConditionalElementFail "Reorder ur predicates"]

            , testCase "returns [TRTp.TRTp.ConditionalFuncApp, TRTp.ConditionalFuncApp, ConditionEval (TRTp.CEArithmetic) (TRTp.CELiteral)] for preCond_arith_2args" $ do
                test_ExprListToRCList progIO "preCond_arith_2args"              [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "j0" "arg0",TRTp.CEBinding "x" "arg1"],TRTp.ConditionalFuncApp "dependents" [TRTp.CEBinding "j1" "arg0",TRTp.CEBinding "y" "arg1"],TRTp.ConditionalEval BCgt (TRTp.CEArithmetic BAadd (TRTp.CEVarExpr . TRTp.LocVar $"x") (TRTp.CEVarExpr . TRTp.LocVar $"y")) (TRTp.CELiteral (IntV 10))]

            , testCase "returns [TRTp.TRTp.ConditionalFuncApp, TRTp.ConditionalFuncApp, ConditionEval (TRTp.CEArithmetic (TRTp.CEArithmetic)) (TRTp.CELiteral)] for preCond_arith_3args" $ do
                test_ExprListToRCList progIO "preCond_arith_3args"              [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "j0" "arg0",TRTp.CEBinding "x" "arg1"],TRTp.ConditionalFuncApp "dependents" [TRTp.CEBinding "j1" "arg0",TRTp.CEBinding "y" "arg1"],TRTp.ConditionalFuncApp "earnings" [TRTp.CEBinding "j2" "arg0",TRTp.CEBinding "z" "arg1",TRTp.CEEquality "arg2" "steady"],TRTp.ConditionalEval BCgt (TRTp.CEArithmetic BAsub (TRTp.CEArithmetic BAadd (TRTp.CEVarExpr . TRTp.LocVar $ "x") (TRTp.CEVarExpr. TRTp.LocVar $ "y")) (TRTp.CEVarExpr. TRTp.LocVar $ "z")) (TRTp.CELiteral (IntV 10))]

            , testCase "fails for preCond_arith_noBinding" $ 
                test_ExprListToRCList progIO "preCond_arith_noBinding"          [TRTp.ConditionalFuncApp "amount_saved" [TRTp.CEBinding "j0" "arg0",TRTp.CEBinding "x" "arg1"],TRTp.ConditionalElementFail "Reorder ur predicates"]
            ]
        , testGroup "Type: RuleAction" 
            [ testGroup "exprToRuleAction" 
                [ expectFail $ testCase "returns ActionExprErr for AppE with GlobalVar" $ do
                    rule <- progToRule progIO "postCond_withGlobal"
                    -- TRTR.exprToRuleAction (postcondOfRule rule) @?= (TRTp.ActionFuncApp "investment" [TRTp.Value "savings"])
                    1 @?= 0
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


test_ExprListToRCList :: IO (Program (Tp ())) -> String -> TRTp.RCList -> IO()
test_ExprListToRCList pIO rName expected = do
    rule <- progToRule pIO rName
    let pc = TRTR.precondToExprList $ precondOfRule rule
    snd (TRTR.exprlistToRCList 0 S.empty [] pc) @?= expected