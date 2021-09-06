{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Syntax hiding (Assertion)
import Language.LSP.Test
import Language.LSP.Types
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import qualified Language.LSP.Types as J
import L4LSP (handleUriErrs, LspError (ReadFileErr))
import Lexer (Err(Err))
import Annotation (SRng(DummySRng))

main :: IO ()
main = defaultMain $ testGroup "Tests" [hoverTests, hoverTypeInfoTests, typeCheckerTests, unitTests]

hoverTests :: TestTree
hoverTests = testGroup "Hover tests"
  -- TODO: update or remove the string lexicon & hover over nothing tests
  [ expectFailBecause "Lexicon is not in AST" $ testHover "mini.l4 hover the string 'lexicon'"                        "mini.l4" (Position 2 4)   (mkRange 2 0 2 7)    "This is a lexicon"
  ,                                             testHover "mini.l4 hover the string 'Business -> \"business_1_N\"'"   "mini.l4" (Position 4 4)   (mkRange 4 0 4 66)   "This block maps variable DetractsFromDignity to GrammaticalFramework WordNet definion \"detracts from dignity of legal profession\""
  ,                                             testHover "cr.l4 hover the string 'class Business {'"                 "cr.l4"   (Position 21 10) (mkRange 21 0 24 1)  "Declaration of new class : Business"
  ,                                             testHover "cr.l4 hover AssociatedWith"                                "cr.l4"   (Position 38 12) (mkRange 38 0 38 64) "Declaration of global variable AssociatedWith"
  ,                                             testHover "cr.l4 hover AssociatedWith type"                           "cr.l4"   (Position 38 30) (mkRange 38 0 38 64) "Declaration of global variable AssociatedWith"
  ,                                             testHover "cr.l4 hover rule r1a"                                      "cr.l4"   (Position 61 7)  (mkRange 61 0 64 29) "Declaration of rule r1a"
  , expectFail                                $ testHover "Hover over nothing"                                        "mini.l4" (Position 1 0)   (mkRange 3 0 3 8)    "This block maps variable Business to WordNet definion \"business_1_N\""
  ]

hoverTypeInfoTests :: TestTree
hoverTypeInfoTests = testGroup "Hover type tests"
  [ testHover "mini.l4 type info for lexicon DetractsFromDignity"   "mini.l4" (Position 4 4)   (mkRange 4 0 4 66)   "OkT"
  , testHover "cr.l4 type info for 'class Business {'"              "cr.l4"   (Position 21 10) (mkRange 21 0 24 1)  "OkT"
  , testHover "cr.l4 type info for var decl AssociatedWith"         "cr.l4"   (Position 38 12) (mkRange 38 0 38 64) "OkT"
  , testHover "cr.l4 type info for rule r1a"                        "cr.l4"   (Position 61 7)  (mkRange 61 0 64 29) $ T.pack $ show $ ClassT () (ClsNm {stringOfClassName = "Boolean"})
  , testHover "cr.l4 type info for rule subexpr MustNotAcceptApp"   "cr.l4"   (Position 69 8)  (mkRange 69 5 69 21) $ T.pack $ show $ FunT () (ClassT () (ClsNm {stringOfClassName = "LegalPractitioner"})) (FunT () (ClassT () (ClsNm {stringOfClassName = "Appointment"})) (ClassT () (ClsNm {stringOfClassName = "Boolean"})))
  ]

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Type Error tests"
  [
    testGroup "ClassDeclsError"
      [
        testTypeErrs "dupClassNames.l4    duplicate class names Business"           "ClassDeclsError/dupClassNames.l4"    (mkRange 2 0 5 1)    2 "Duplicate names in class declarations:\nAt (2,0) .. (5,1) class name Business"
      , testTypeErrs "undefSuperClass.l4  undefined super class LawFirm"            "ClassDeclsError/undefSuperClass.l4"  (mkRange 3 0 3 30)   1 "Undefined superclasses in the following class declarations:\nAt (3,0) .. (3,30) class name LawFirm"
      -- Note: typeErrTests only checks the first error, however, this next test in particular returns 2 errors; one for LawFirm, one for Business
      , testTypeErrs "cyclicClass.l4      cyclic classes LawFirm & Business"        "ClassDeclsError/cyclicClass.l4"      (mkRange 0 0 0 30)   2 "Cyclic class references in the following class declarations:\nAt (0,0) .. (0,30) class name LawFirm\nAt (2,0) .. (2,30) class name Business"
      ]
  , testGroup "FieldDeclsError"
      [
        testTypeErrs "dupFieldNames.l4    duplicate field name for class Business"  "FieldDeclsError/dupFieldNames.l4"    (mkRange 2 0 5 1)    1 "Duplicate field names in the following classes:\nAt (2,0) .. (5,1) class name Business\nAt (3,6) .. (3,18) field name foo\nAt (4,6) .. (4,18) field name foo"
      -- NOTE: This error longer exists
      -- , testTypeErrs "undefFieldType.l4   undefined field type for class Business"  "FieldDeclsError/undefFieldType.l4"   (mkRange 3 11 3 17)   1 "Class name Entity at (3,11) .. (3,17) is undefined."
      ]
  , testGroup "VarDeclsError"
      [
        testTypeErrs "undeclVar.l4        undefined variable MustNotAcceptApp"      "VarDeclsError/undeclVar.l4"          (mkRange 64 5 64 21) 7 "Variable MustNotAcceptApp at (64,5) .. (64,21) is undefined"
      -- NOTE: This error longer exists
      -- , testTypeErrs "undefVarType.l4     undefined variable type Entity"           "VarDeclsError/undefVarType.l4"       (mkRange 2 0 2 33)   1 "Undefined type var decl: WhereType"
      ]
  , testGroup "RuleError"
      [
        testTypeErrs "undeclVar.l4            undefined variable AssociatedWithAppB"                    "RuleAssertionError/undeclVar.l4"             (mkRange 6 27 6 45)   3 "Variable AssociatedWithAppB at (6,27) .. (6,45) is undefined."
      , testTypeErrs "illTypedSubExpr.l4      has type Boolean but a subtype of Number was expected"    "RuleAssertionError/illTypedSubExpr.l4"       (mkRange 1 4  1 12)   2 "has type Boolean but a subtype of Number was expected"
      , testTypeErrs "incompatibleTps.l4      types are not compatible"                                 "RuleAssertionError/incompatibleTps.l4"       (mkRange 1 3  1 14)   3 "The types are not compatible (one is subtype of the other)"
      , testTypeErrs "nonScalarTps.l4         at least one type is non-scalar"                          "RuleAssertionError/nonScalarTps.l4"          (mkRange 4 3  4 24)   3 "At least one type is not scalar (non-functional)"
      , testTypeErrs "nonFunctionTp.l4        which is not a functional type"                           "RuleAssertionError/nonFunctionTp.l4"         (mkRange 7 39 7 46)   2 "which is not a functional type"
      -- Tuple patterns no longer exists
      -- , testTypeErrs "incompatiblePattern.l4  variable pattern & expected type are incompatible"        "RuleAssertionError/incompatiblePattern.l4"   (mkRange 1 3  1 34)   1 "the variable pattern and its type are incompatible"
      , testTypeErrs "unknownFieldName.l4  access to an unknown field"                                  "RuleAssertionError/unknownFieldName.l4"      (mkRange 10 8 10 9)   1 "access to an unknown field"
      , testTypeErrs "accessToNonObjectTp.l4  access to a field of a non-object type"                   "RuleAssertionError/accessToNonObjectTp.l4"   (mkRange 3 4 3 5)     1 "access to an unknown field De in class Boolean"
      ]
  ]


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
    testCase "handleUriErrs returns ReadFileErr with non-existent file" $ do
      handleUriErrs (Uri "doesNotExist.l4") @?= Left (ReadFileErr $ "Unable to parse uri: \"doesNotExist.l4\"")
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
    runSession "lsp-server-bl4" fullCaps "lsp-tests/examples/hoverError" $ do
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
  :: HasCallStack
  => TestName
  -> FilePath -- ^ The file to test
  -> Range -- ^ expected range of error (0 - indexed)
  -> Int    -- ^ number of errors expected        -- TODO: maybe take a list of errors to check for instead?
  -> T.Text -- ^ expected error message
  -> TestTree
testTypeErrs testName fileName expectedRange numDiags containedText =
    testCase ("lsp-tests/examples/" ++ fileName ++ "\t"++ testName) $ do
    runSession "lsp-server-bl4" fullCaps "lsp-tests/examples" $ do
        doc <- openDoc fileName "l4"
        diags <- waitForDiagnostics
        liftIO $ assertEqual "Wrong number of diagnostics" numDiags $ length diags
        let J.Diagnostic
                  range
                  _
                  _
                  _
                  errMsg
                  _
                  _
                 = head diags

        liftIO $ assertEqual ("Wrong range for error message: " ++ show errMsg) expectedRange range
        liftIO $ assertFoundIn "type error" containedText errMsg
        pure ()

-- | Assert that the first text should be a substring of the second
assertFoundIn :: HasCallStack => T.Text -> T.Text -> T.Text -> Assertion
assertFoundIn whrFrom part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in " <> whrFrom <> ":\n" <> whole)
    (part `T.isInfixOf` whole)

