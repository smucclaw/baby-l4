module ExpSysTest where

import Syntax
import Parser ( parseProgram )
import Annotation
import MainHelpers ( readPrelude )
import qualified SimpleRules as SR
import Test.Tasty
import Test.Tasty.HUnit


-- esUnitTests :: IO ()
-- esUnitTests = putStrLn "Import works"

esUnitTests :: Program (Tp ()) -> IO () 
esUnitTests prog = defaultMain $ testGroup "Expert System Tests" [
        testGroup "isRule" [
            testCase "returns True for basicRule.l4" $ do
                let rule = head $ getRule prog "accInad"
                SR.isRule rule @?= True    
        ]
    ]


getProg :: FilePath -> IO (Program (Tp ()))
getProg fpath = do
    contents <- readFile fpath
    case parseProgram fpath contents of 
        -- Left err -> do
        --     putStrLn "Parse Error: "
        --     print err
        Right ast -> do

    

getRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
getRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]