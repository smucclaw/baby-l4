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
import qualified Language.LSP.Types as J

main :: IO ()
main = defaultMain $ testGroup "Tests" [hoverTests, typeCheckerTests]

hoverTests :: TestTree
hoverTests = testGroup "Hover tests"
  -- TODO: update the string lexicon & hover over nothing tests
  [ expectFailBecause "Lexicon is not in AST" $ testHover "mini.l4 hover the string 'lexicon'"                        "mini.l4" (Position 2 4)   (mkRange 2 0 2 7)    "This is a lexicon"
  ,                                             testHover "mini.l4 hover the string 'Business -> \"business_1_N\"'"   "mini.l4" (Position 4 4)   (mkRange 4 0 4 66)   "This block maps variable DetractsFromDignity to GrammaticalFramework WordNet definion \"detracts from dignity of legal profession\""
  ,                                             testHover "cr.l4 hover the string 'class Business {'"                 "cr.l4"   (Position 21 10) (mkRange 21 0 24 1)  "Declaration of new class : Business"
  ,                                             testHover "cr.l4 hover AssociatedWith"                                "cr.l4"   (Position 38 12) (mkRange 38 0 38 64) "Declaration of global variable AssociatedWith"
  ,                                             testHover "cr.l4 hover AssociatedWith type"                           "cr.l4"   (Position 38 30) (mkRange 38 0 38 64) "Declaration of global variable AssociatedWith"
  ,                                             testHover "cr.l4 hover rule r1a"                                      "cr.l4"   (Position 61 7)  (mkRange 61 0 64 29) "Declaration of rule r1a"
  , expectFail                                $ testHover "Hover over nothing"                                        "mini.l4" (Position 1 0)   (mkRange 3 0 3 8)    "This block maps variable Business to WordNet definion \"business_1_N\""
  ]

typeCheckerTests :: TestTree 
typeCheckerTests = testGroup "Type Error tests"  
  [
    testGroup "ClassDeclsError"
      [
        testTypeErrs "dupClassNames.l4    duplicate class names Business"           "ClassDeclsError/dupClassNames.l4"    (mkRange 2 0 5 1)    2 "Duplicate class name: Business"
      , testTypeErrs "undefSuperClass.l4  undefined super class LawFirm"            "ClassDeclsError/undefSuperClass.l4"  (mkRange 3 0 3 30)   1 "Undefined super class: LawFirm"
      -- Note: typeErrTests only checks the first error, however, this next test in particular returns 2 errors; one for LawFirm, one for Business
      , testTypeErrs "cyclicClass.l4      cyclic classes LawFirm & Business"        "ClassDeclsError/cyclicClass.l4"      (mkRange 0 0 0 30)   2 "Cyclic class hierarchy: LawFirm" 
      ]
  , testGroup "FieldDeclsError"
      [
        testTypeErrs "dupFieldNames.l4    duplicate field name for class Business"  "FieldDeclsError/dupFieldNames.l4"    (mkRange 2 0 5 1)    1 "Duplicate field names in the class: Business"
      , testTypeErrs "undefFieldType.l4   undefined field type for class Business"  "FieldDeclsError/undefFieldType.l4"   (mkRange 3 6 3 17)   1 "Undefined field type: foo"
      ] 
  , testGroup "VarDeclsError"
      [
        testTypeErrs "undeclVar.l4        undefined variable MustNotAcceptApp"      "VarDeclsError/undeclVar.l4"          (mkRange 64 5 64 21) 6 "Variable MustNotAcceptApp at (64,5) .. (64,21) is undefined"
      , testTypeErrs "undefVarType.l4     undefined variable type Entity"           "VarDeclsError/undefVarType.l4"       (mkRange 2 0 2 33)   1 "Undefined type var decl: WhereType"
      ]
  , testGroup "RuleError"
      [
        testTypeErrs "undeclVar.l4        undefined variable AssociatedWithAppB"    "RuleAssertionError/undeclVar.l4"      (mkRange 2 27 2 45)  3 "Rule Error: Variable AssociatedWithAppB"
      , testTypeErrs "nonScalarTps.l4     at least one type is non-scalar"          "RuleAssertionError/nonScalarTps.l4"   (mkRange 4 3  4 20)  1 "At least one type is not scalar (non-functional)"
      ]
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
        -- liftIO $ print (hoverText, hoverRange)
        liftIO $ hoverRange @?= Just expectedRange
        liftIO $ assertFoundIn "hover message" containedText hoverText
        pure ()

-- test hwat happens if we edit a doc, fix an error & see if error is gone

-- test sequence of edits
testTypeErrs 
  :: TestName 
  -> FilePath -- ^ The file to test
  -> Range -- ^ expected range of error (0 - indexed)
  -> Int    -- ^ number of errors expected        -- TODO: maybe take a list of errors to check for instead? 
  -> T.Text -- ^ expected error message
  -> TestTree
testTypeErrs testName fileName expectedRange numDiags containedText =
    testCase testName $ do
    runSession "lsp-server-bl4" fullCaps "lsp-tests/examples" $ do
        doc <- openDoc fileName "l4"
        diags <- waitForDiagnostics
        liftIO $ diags @?= []
        diags <- waitForDiagnostics
        liftIO $ length diags @?= numDiags
        let J.Diagnostic
                  range
                  _
                  _
                  _
                  errMsg
                  _
                  _
                 = head diags
        
        liftIO $ range @?= expectedRange
        liftIO $ assertFoundIn "type error" containedText errMsg
        pure ()

-- | Assert that the first text should be a substring of the second
assertFoundIn :: T.Text -> T.Text -> T.Text -> Assertion
assertFoundIn whrFrom part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in " <> whrFrom <> ":\n" <> whole)
    (part `T.isInfixOf` whole)

