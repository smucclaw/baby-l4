{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import L4.Syntax hiding (Assertion)
import Language.LSP.Test
import Language.LSP.Types
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import qualified Language.LSP.Types as J
import L4.L4LSP (handleUriErrs, LspError (ReadFileErr), realSRngToRange, posToPosition)
import L4.Lexer (Err(Err))
import L4.Annotation (SRng(DummySRng), Pos (Pos), RealSRng(..))

main :: IO ()
main = defaultMain $ testGroup "Tests" [hoverTests, hoverTypeInfoTests, typeCheckerTests, unitTests]

-- | Takes 1-indexed range positions and converts them to 0-indexed range positions for LSP-server
mkRange' :: Int -> Int -> Int -> Int -> RealSRng
mkRange' l1 c1 l2 c2 = SRng (Pos l1 c1) (Pos l2 c2)

mkPosition :: Int -> Int -> Pos
mkPosition = Pos

hoverTests :: TestTree
hoverTests = testGroup "Hover tests"
  [ testHover "mini.l4 hover the DetractsFromDignity lexicon entry"       "mini.l4" (mkPosition 5 5)   (mkRange' 5 1 5 67)   "This block maps variable DetractsFromDignity to CNL description \"detracts from dignity of legal profession\""
  , testHover "cr.l4 hover the string 'class Business {'"                 "cr.l4"   (mkPosition 22 11) (mkRange' 22 1 25 2)  "Declaration of new class : Business"
  , testHover "cr.l4 hover AssociatedWith"                                "cr.l4"   (mkPosition 39 13) (mkRange' 39 1 39 65) "Declaration of global variable AssociatedWith"
  , testHover "cr.l4 hover AssociatedWith type"                           "cr.l4"   (mkPosition 39 31) (mkRange' 39 1 39 65) "Declaration of global variable AssociatedWith"
  , testHover "cr.l4 hover rule r1a"                                      "cr.l4"   (mkPosition 62 8)  (mkRange' 62 1 65 30) "Declaration of rule r1a"
  ]

hoverTypeInfoTests :: TestTree
hoverTypeInfoTests = testGroup "Hover type tests"
  [ testHover "mini.l4 type info for lexicon DetractsFromDignity"   "mini.l4" (mkPosition 5 5)   (mkRange' 5 1 5 67)   "OkT"
  , testHover "cr.l4 type info for 'class Business {'"              "cr.l4"   (mkPosition 22 11) (mkRange' 22 1 25 2)  "OkT"
  , testHover "cr.l4 type info for var decl AssociatedWith"         "cr.l4"   (mkPosition 39 13) (mkRange' 39 1 39 65) "OkT"
  , testHover "cr.l4 type info for rule r1a"                        "cr.l4"   (mkPosition 62 8)  (mkRange' 62 1 65 30) $ T.pack $ show $ ClassT () (ClsNm {stringOfClassName = "Boolean"})
  , testHover "cr.l4 type info for rule subexpr MustNotAcceptApp"   "cr.l4"   (mkPosition 70 9)  (mkRange' 70 6 70 22) $ T.pack $ show $ FunT () (ClassT () (ClsNm {stringOfClassName = "LegalPractitioner"})) (FunT () (ClassT () (ClsNm {stringOfClassName = "Appointment"})) (ClassT () (ClsNm {stringOfClassName = "Boolean"})))
  ]

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Type Error tests"
  [
    testGroup "ClassDeclsError"
      [
        testTypeErrs "dupClassNames.l4    duplicate class names Business"           "ClassDeclsError/dupClassNames.l4"    (mkRange' 3 1 6 2)    2 "Duplicate names in class declarations:\nAt (3,1) .. (6,2) class name Business"
      , testTypeErrs "undefSuperClass.l4  undefined super class LawFirm"            "ClassDeclsError/undefSuperClass.l4"  (mkRange' 4 1 4 31)   1 "Undefined superclasses in the following class declarations:\nAt (4,1) .. (4,31) class name LawFirm"
      , testTypeErrs "cyclicClass.l4      cyclic classes LawFirm & Business"        "ClassDeclsError/cyclicClass.l4"      (mkRange' 1 1 1 31)   2 "Cyclic class references in the following class declarations:\nAt (1,1) .. (1,31) class name LawFirm\nAt (3,1) .. (3,31) class name Business"
      ]
  , testGroup "FieldDeclsError"
      [
        testTypeErrs "dupFieldNames.l4    duplicate field name for class Business"  "FieldDeclsError/dupFieldNames.l4"    (mkRange' 3 1 6 2)    1 "Duplicate field names in the following classes:\nAt (3,1) .. (6,2) class name Business\nAt (4,7) .. (4,19) field name foo\nAt (5,7) .. (5,19) field name foo"
      -- NOTE: This error longer exists
      -- , testTypeErrs "undefFieldType.l4   undefined field type for class Business"  "FieldDeclsError/undefFieldType.l4"   (mkRange' 4 12 4 18)   1 "Class name Entity at (3,11) .. (3,17) is undefined."
      ]
  , testGroup "VarDeclsError"
      [
        testTypeErrs "undeclVar.l4        undefined variable MustNotAcceptApp"      "VarDeclsError/undeclVar.l4"          (mkRange' 65 6 65 22) 7 "Variable MustNotAcceptApp at (65,6) .. (65,22) is undefined"
      -- NOTE: This error longer exists
      -- , testTypeErrs "undefVarType.l4     undefined variable type Entity"           "VarDeclsError/undefVarType.l4"       (mkRange' 3 1 3 34)   1 "Undefined type var decl: WhereType"
      ]
  , testGroup "RuleError"
      [
        testTypeErrs "undeclVar.l4            undefined variable AssociatedWithAppB"                    "RuleAssertionError/undeclVar.l4"             (mkRange' 7 28 7 46)   3 "Variable AssociatedWithAppB at (7,28) .. (7,46) is undefined."
      , testTypeErrs "illTypedSubExpr.l4      has type Boolean but a subtype of Number was expected"    "RuleAssertionError/illTypedSubExpr.l4"       (mkRange' 2 5  2 13)   2 "has type Boolean but a subtype of Number was expected"
      , testTypeErrs "incompatibleTps.l4      types are not compatible"                                 "RuleAssertionError/incompatibleTps.l4"       (mkRange' 2 4  2 15)   3 "The types are not compatible (one is subtype of the other)"
      , testTypeErrs "nonScalarTps.l4         at least one type is non-scalar"                          "RuleAssertionError/nonScalarTps.l4"          (mkRange' 5 4  5 25)   2 "At least one type is not scalar (non-functional)"
      , testTypeErrs "nonFunctionTp.l4        which is not a functional type"                           "RuleAssertionError/nonFunctionTp.l4"         (mkRange' 8 40 8 47)   2 "which is not a functional type"
      -- Tuple patterns no longer exists
      -- , testTypeErrs "incompatiblePattern.l4  variable pattern & expected type are incompatible"        "RuleAssertionError/incompatiblePattern.l4"   (mkRange' 2 4  2 35)   1 "the variable pattern and its type are incompatible"
      , testTypeErrs "unknownFieldName.l4  access to an unknown field"                                  "RuleAssertionError/unknownFieldName.l4"      (mkRange' 11 9 11 10)   1 "access to an unknown field"
      , testTypeErrs "accessToNonObjectTp.l4  access to a field of a non-object type"                   "RuleAssertionError/accessToNonObjectTp.l4"   (mkRange' 4 5 4 6)     1 "access to an unknown field De in class Boolean"
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
testHover
  :: HasCallStack
  => TestName -- ^ Description of the test case
  -> FilePath -- ^ The file to test
  -> Pos -- ^ Where in the file to hover (0-indexed)
  -> RealSRng -- ^ Which range should highlight when you hover
  -> T.Text -- ^ A string that should be contained in the hover message
  -> TestTree
testHover testName filename position expectedRange containedText =
  testCase testName $ do
    runSession "lsp-server-bl4" fullCaps "lsp-tests/examples/hoverError" $ do
        doc <- openDoc filename "l4"
        diags <- waitForDiagnostics
        hover <- getHover doc $ posToPosition position
        (hoverText, hoverRange) <- case hover of
            Nothing -> liftIO . assertFailure $ "Got no hover"
            Just
              Hover{_contents = (HoverContents MarkupContent{_value = msg})
                   ,_range    = rangeInHover } -> pure (msg, rangeInHover)
            (Just hov) -> liftIO . assertFailure $ "Unexpected hover type: " ++ show hov
        -- liftIO $ print (hoverText, hoverRange)
        liftIO $ hoverRange @?= Just (realSRngToRange expectedRange)
        liftIO $ assertFoundIn "hover message" containedText hoverText
        pure ()

-- test hwat happens if we edit a doc, fix an error & see if error is gone

-- test sequence of edits
testTypeErrs
  :: HasCallStack
  => TestName
  -> FilePath -- ^ The file to test
  -> RealSRng -- ^ expected range of error (0 - indexed)
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

        liftIO $ assertEqual ("Wrong range for error message: " ++ show errMsg) (realSRngToRange expectedRange) range
        liftIO $ assertFoundIn "type error" containedText errMsg
        pure ()

-- | Assert that the first text should be a substring of the second
assertFoundIn :: HasCallStack => T.Text -> T.Text -> T.Text -> Assertion
assertFoundIn whrFrom part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in " <> whrFrom <> ":\n" <> whole)
    (part `T.isInfixOf` whole)

