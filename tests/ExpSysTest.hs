module ExpSysTest where

import Syntax
import Annotation
import MainHelpers ( getTpAst )

import Control.Monad.Except (runExceptT)
import qualified SimpleRules as SR
import qualified ToRules as TR
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either ( rights )
import Data.Set as S



-- given a filepath, return a program
getProg :: FilePath -> IO (NewProgram (Tp ()))
getProg fpath = do
    contents <- readFile fpath
    errOrTpAst <- runExceptT $ getTpAst fpath contents
    case errOrTpAst of
        Right prog -> return $ typeAnnot <$> prog
        Left errs -> do
            error $ show errs -- TODO: Add error printing with proper formatting
 
-- given rulename, filter out rule from program
obtRule :: NewProgram (Tp ()) -> String -> [Rule (Tp ())]
obtRule prog rname = [r | r <- rulesOfNewProgram prog, nameOfRule r == Just rname ]




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
esRuleUTs = testGroup "Expert System: Rule Translation Tests" -- todo: fill in the rest
    [ testGroup "exprToCEArg"
        [ testCase "returns CEBinding for a LocalVar expr" $ do
            TR.exprToCEArg (0, VarE () (LocalVar (QVarName () "foo") 0)) @?= TR.CEBinding "foo" "arg0"
        , testCase "returns CEEquality for a GlobalVar expr" $ do 
            TR.exprToCEArg (0, VarE () (GlobalVar (QVarName () "foo"))) @?= TR.CEEquality "arg0" "foo"
        ]
    ]