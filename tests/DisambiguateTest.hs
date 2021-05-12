module DisambiguateTest where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.ExpectedFailure
-- import Control.Monad.IO.Class (MonadIO(liftIO))
import PGF
import Paths_baby_l4 (getDataFileName)
import ToGF.Disambiguate
import qualified Data.Map as M

disambiguateTests :: PGF -> TestTree
disambiguateTests pgf = testGroup "Disambiguation questions"
    [ unauthorizedSharingFees
    ]
  where

  unauthorizedSharingFees :: TestTree
  unauthorizedSharingFees = testCase "UnauthorizedSharingFees" $ do
      [opt1, _opt2] <- "involves sharing fees with unauthorized persons" `strShouldDisambiguateTo` ["party X involves a sharing-fee", "party X shares fees"]
      _ <- opt1 `exprsShouldDisambiguateTo` ["party X involves a sharing-fee"]
      -- length opt1 @?= 1

      pure ()

  strShouldDisambiguateTo :: String -> [String] -> IO [[Expr]]
  strShouldDisambiguateTo input expected = do
      let lang = head $ languages pgf
          exprs = parse pgf lang (startCat pgf) input
      exprs `exprsShouldDisambiguateTo` expected

  exprsShouldDisambiguateTo :: [Expr] -> [String] -> IO [[Expr]]
  exprsShouldDisambiguateTo input expected = do
      let output = generateQuestions pgf input
      map fst output @?= expected
      pure $ map snd output


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
