{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Language.LSP.Test
import Language.LSP.Types
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "Tests" [hoverTests, example]

hoverTests :: TestTree
hoverTests = testGroup "Hover tests"
  [ 
    -- testHover "mini.l4 hover LegalPractitioner"         "mini.l4" (Position 20 10) (mkRange 19 0 22 28) "TokenSym \"LegalPractitioner\"",
    testHover "mini.l4 hover the string 'lexicon'" "mini.l4" (Position 2 4)  (mkRange 2 0 2 7)    "This is a lexicon"
  , testHover "mini.l4 hover the string 'Business -> \"business_1_N\"'" "mini.l4" (Position 3 4)  (mkRange 3 0 3 26)    "This block maps variable Business to GrammaticalFramework WordNet definion \"business_1_N\""
  , testHover "cr.l4 hover the string 'class Business {'" "cr.l4" (Position 21 10)  (mkRange 21 0 24 1)    "Declaration of new class : Business"
  , testHover "cr.l4 hover AssociatedWith" "cr.l4" (Position 38 12)  (mkRange 38 0 38 20)    "Declaration of global variable AssociatedWith"
  , testHover "cr.l4 hover rule r1a" "cr.l4" (Position 61 7)  (mkRange 61 0 64 29)    "Declaration of rule r1a"
  , expectFail $ testHover "Hover over nothing" "mini.l4" (Position 1 0)  (mkRange 3 0 3 8)    "This block maps variable Business to WordNet definion \"business_1_N\""
  ]

-- TODO: We might want to test several hovers for a single file at once,
-- but currently each hover will reparse the file anyways, so it won't make
-- any performance difference anyways.
testHover :: TestName -- ^ Description of the test case
  -> FilePath -- ^ The file to test
  -> Position -- ^ Where in the file to hover (0-indexed)
  -> Range -- ^ Which range should highlight when you hover
  -> T.Text -- ^ A string that should be contained in the hover message
  -> TestTree
testHover testName filename position expectedRange containedText =
  testCase testName $ do
    runSession "lsp-server-bl4" fullCaps "l4" $ do
        doc <- openDoc filename "l4"
        diags <- waitForDiagnostics
        hover <- getHover doc position
        (hoverText, hoverRange) <- case  hover of
            Nothing -> liftIO . assertFailure $ "Got no hover"
            Just
              Hover{_contents = (HoverContents MarkupContent{_value = msg})
                   ,_range    = rangeInHover } -> pure (msg, rangeInHover)
        -- liftIO $ print (hoverText, hoverRange)
        liftIO $ hoverRange @?= Just expectedRange
        liftIO $ assertFoundIn containedText hoverText
        pure ()

-- | Assert that the first text should be a substring of the second
assertFoundIn :: T.Text -> T.Text -> Assertion
assertFoundIn part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
    (part `T.isInfixOf` whole)

example =
  testCaseSteps "Example test case" $ \step -> do
    -- assertion no. 1 (passes)
    step "hello"
    2 + 2 @?= 4
    -- assertion no. 2 (fails)
    step "world"
    -- assertBool "the list is not empty" $ null [1]
    -- assertion no. 3 (would have failed, but won't be executed because
    -- the previous assertion has already failed)
    step "foobar"
    -- "foo" @?= "bar"
