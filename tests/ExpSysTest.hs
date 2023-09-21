module ExpSysTest where

import L4.Syntax
import L4.Annotation
import MainHelpers ( getTpAst )

import Control.Monad.Except (runExceptT)
import SimpleRules qualified as SR
import ToRules.ToRules qualified as TRTR
import ToRules.Types qualified as TRTp
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
    testGroup "Expert System: Rule Translation Tests" [ ]
    where acquire = getProg "tests/ExpSysTestFiles/finAd_tests.l4"
          release = mempty


test_ExprListToRCList :: IO (Program (Tp ())) -> String -> TRTp.RCList -> IO()
test_ExprListToRCList pIO rName expected = do
    rule <- progToRule pIO rName
    let pc = TRTR.precondToExprList $ precondOfRule rule
    snd (TRTR.exprlistToRCList 0 S.empty [] pc) @?= expected