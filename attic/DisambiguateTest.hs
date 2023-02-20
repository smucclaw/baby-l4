{-# LANGUAGE TupleSections #-}
module DisambiguateTest where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.ExpectedFailure
-- import Control.Monad.IO.Class (MonadIO(liftIO))
import PGF
import Paths_baby_l4 (getDataFileName)
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
    , "walks in legal work at home" `shouldDisambiguateTo` ["higher education"]
    , findAdjunctsTest
    ]
  where

  findAdjunctsTest = testGroup "Find adjuncts"
    [ testCase "Basic tests" $ do
        let ex1 = parseStr "walks in legal work"
        map (showExpr []) ex1 @?= ["p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (UseV walk_V) (PrepNP in_Prep (MassNP (AdjCN (PositA legal_A) (UseN work_N)))))))"]
        fmap (map show . findAdjuncts . fg') ex1 @?= [["PrepNP in_Prep (MassNP (AdjCN (PositA legal_A) (UseN work_N)))"]]

        let ex2 = map snd $ filterHeuristic 0 $ map ((),) $ parseStr "walks in legal work at home"
        map (showExpr []) ex2 @?=
            ["p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (UseV walk_V) (PrepNP in_Prep (MassNP (AdjCN (PositA legal_A) (UseN (CompoundN work_N at_home_N))))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (UseV walk_V) (PrepNP in_Prep (MassNP (AdvCN (AdjCN (PositA legal_A) (UseN work_N)) at_home_Adv))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (AdvVP (UseV walk_V) (PrepNP in_Prep (MassNP (AdjCN (PositA legal_A) (UseN work_N))))) at_home_Adv)))"]
        fmap (map show . findAdjuncts . fg') ex2 @?=
            [["PrepNP in_Prep (MassNP (AdjCN (PositA legal_A) (UseN (CompoundN work_N at_home_N))))"]
            ,["PrepNP in_Prep (MassNP (AdvCN (AdjCN (PositA legal_A) (UseN work_N)) at_home_Adv))","at_home_Adv"]
            ,["at_home_Adv","PrepNP in_Prep (MassNP (AdjCN (PositA legal_A) (UseN work_N)))"]]

        let ex3 = map snd $ filterHeuristic 0 $ map ((),) $ parseStr "involves sharing fees with sole proprietors"
        map (showExpr []) ex3 @?=
            ["p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (ComplSlash (SlashV2a involve_V2) (MassNP (GerundCN (ComplSlash (SlashV2a share_V2) (DetCN (DetQuant IndefArt NumPl) (UseN fee_N)))))) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (ComplSlash (SlashV2a involve_V2) (MassNP (GerundCN (ComplSlash (SlashV2a share_V2) (DetCN (DetQuant IndefArt NumPl) (UseN fee_N)))))) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (ComplSlash (SlashV2a involve_V2) (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sharing_N fee_N)))) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (AdvVP (ComplSlash (SlashV2a involve_V2) (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sharing_N fee_N)))) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (ComplSlash (SlashV2a involve_V2) (MassNP (GerundCN (AdvVP (ComplSlash (SlashV2a share_V2) (DetCN (DetQuant IndefArt NumPl) (UseN fee_N))) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N))))))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (ComplSlash (SlashV2a involve_V2) (MassNP (GerundCN (AdvVP (ComplSlash (SlashV2a share_V2) (DetCN (DetQuant IndefArt NumPl) (UseN fee_N))) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N))))))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (ComplSlash (SlashV2a involve_V2) (MassNP (GerundCN (ComplSlash (SlashV2a share_V2) (DetCN (DetQuant IndefArt NumPl) (AdvCN (UseN fee_N) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N))))))))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (ComplSlash (SlashV2a involve_V2) (MassNP (GerundCN (ComplSlash (SlashV2a share_V2) (DetCN (DetQuant IndefArt NumPl) (AdvCN (UseN fee_N) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N))))))))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (ComplSlash (SlashV2a involve_V2) (DetCN (DetQuant IndefArt NumPl) (AdvCN (UseN (CompoundN sharing_N fee_N)) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))))))))"
            ,"p1 (ComplVP (MkVPS (TTAnt TPres ASimul) PPos (ComplSlash (SlashV2a involve_V2) (DetCN (DetQuant IndefArt NumPl) (AdvCN (UseN (CompoundN sharing_N fee_N)) (PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))))))))"]
        fmap (map show . findAdjuncts . fg') ex3 @?=
            [["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (AdjCN (PositA sole_A) (UseN proprietor_N)))"]
            ,["PrepNP with_Prep (DetCN (DetQuant IndefArt NumPl) (UseN (CompoundN sole_N proprietor_N)))"]]

        pure ()
    ]
-- walks
--   in legal (work at home)
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

  parseStr = parse pgf lang (startCat pgf)
      where lang = head $ languages pgf

  strShouldDisambiguateTo :: Partial => String -> [String] -> IO [[Expr]]
  strShouldDisambiguateTo input expected = do
      parseStr input `exprsShouldDisambiguateTo` expected

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
