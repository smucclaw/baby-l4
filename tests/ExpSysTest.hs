module ExpSysTest where

import Syntax
import Annotation
import MainHelpers ( readPrelude, getTpAst )

import Control.Monad.Except (runExceptT)
import qualified SimpleRules as SR
import Test.Tasty
import Test.Tasty.HUnit



-- esUnitTests :: IO ()
-- esUnitTests = putStrLn "Import works"

esUnitTests :: TestTree
esUnitTests = withResource acquire release $ \progIO->
    testGroup "Expert System Tests"
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
        ]
    where acquire = getProg "tests/ExpSysTestFiles/financial_advisor.l4"
          release = mempty
          progToRule progIO rname = do
              prog <- progIO
              pure $ head $ getRule prog rname


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
getRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
getRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]