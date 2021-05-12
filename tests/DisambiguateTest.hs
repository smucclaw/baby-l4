module DisambiguateTest where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.ExpectedFailure
-- import Control.Monad.IO.Class (MonadIO(liftIO))
import PGF
import Paths_baby_l4 (getDataFileName)
import ToGF.Disambiguate
import qualified Data.Map as M
import Control.Monad (forM_, void)
import Safe.Partial (Partial)

disambiguateTests :: PGF -> TestTree
disambiguateTests pgf = testGroup "Disambiguation questions"
    [
      unauthorizedSharingFees
    , prohibited
    , "involves sharing fees with unauthorized persons" `shouldDisambiguateTo` ["party X involves sharing-fees", "party X shares fees"]
    -- , longSharingFees
    , "sole independent proprietor" `shouldDisambiguateTo` ["higher education","party X is a sole independent proprietor","party X is a sole independent-proprietor","party X is an independent proprietor","party X soles independent-proprietor"] -- Triple compound or soles
    ]
  where

  prohibited :: TestTree
  prohibited = testCase "Prohibited" $ do
      [_,_,_] <- "prohibited" `strShouldDisambiguateTo` ["party X is prohibited","party X prohibits", -- Business : Bool
                                                    "party X prohibits party Y"] -- : Business -> Something -> Bool
      pure ()

  unauthorizedSharingFees :: TestTree
  unauthorizedSharingFees = testCase "UnauthorizedSharingFees" $ do
      [opt1, opt2] <- "involves sharing fees with a sole independent proprietor" `strShouldDisambiguateTo` ["party X involves sharing-fees", "party X shares fees"]
    --   mapM_ print opt1
      _ <- opt1 `exprsShouldDisambiguateTo` ["party X involves sharing-fees"]
      _ <- opt2 `exprsShouldDisambiguateTo` ["party X shares fees"]
      -- length opt1 @?= 1

      pure ()

--   longSharingFees :: TestTree
--   longSharingFees = testCase "Payment of comissions" $ do
--       [] <- "involves sharing fees for legal work by unauthorized persons performed by the legal practioner" `strShouldDisambiguateTo` []
--       pure ()
--   -- "involves sharing fees for legal work by unauthoirzed persons performed by the legal practitioner"




  -- | Top level helper
  shouldDisambiguateTo :: Partial => String -> [String] -> TestTree
  shouldDisambiguateTo input expected = testCase input $ void $ input `strShouldDisambiguateTo` expected

  strShouldDisambiguateTo :: Partial => String -> [String] -> IO [[Expr]]
  strShouldDisambiguateTo input expected = do
      let lang = head $ languages pgf
          exprs = parse pgf lang (startCat pgf) input
      exprs `exprsShouldDisambiguateTo` expected

  exprsShouldDisambiguateTo :: Partial => [Expr] -> [String] -> IO [[Expr]]
  exprsShouldDisambiguateTo input expected = do
      let output = generateQuestions pgf input
      forM_ [] putStrLn
    --   let foo = mkQuestions' input
    --   forM_ (filterHeuristic 0 foo) $ \(qs, e) -> do
    --     putStrLn ""
    --     forM_ qs $ print . fmap (linearizeQuestion pgf)
    --     putStrLn $ showExpr [] e
    --     putStrLn ""
      map fst output @?= expected
      pure $ map snd output

--

generateQuestions :: PGF -> [Expr] -> [(String, [Expr])]
generateQuestions pgf exprs = M.toList $ mkQuestionMap pgf qs
  where
    qs = mkQuestions exprs


{-
Input:

decl UnauthorizedSharingFees : Business -> Bool
lexicon
UnauthorizedSharingFees -> "involves sharing fees with unauthorized persons"

Output:

["the business involves sharing - fees"
,"the business shares fees"]

-}

-- extractSubject :: TypeSignature -> String

getPgf :: IO PGF
getPgf = do
    parsepgfName <- getDataFileName "grammars/ParsePredicates.pgf"
    PGF.readPGF parsepgfName
