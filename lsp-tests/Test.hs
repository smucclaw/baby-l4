{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Language.LSP.Test
import Language.LSP.Types
import Control.Monad.IO.Class (MonadIO(liftIO))

main = defaultMain $ testGroup "Tests" [lspExample, example]

lspExample =
  testCase "lspExample" $ do
    runSession "lsp-server-bl4" fullCaps "l4" $ do
        doc <- openDoc "mini.l4" "l4"
        diags <- waitForDiagnostics
        let pos = Position 12 5
            params = TextDocumentPositionParams doc
            hoverParams = HoverParams doc pos Nothing
        hover <- request STextDocumentHover hoverParams
        extractedHover <- case _result hover of
            Left e -> liftIO . assertFailure $ "Got error: " ++ show e
            Right Nothing -> liftIO . assertFailure $ "Got no hover"
            Right (Just x) -> pure x
        liftIO $ print extractedHover
        pure ()

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

{-
Stolen from haskell-language-server tests:


  checkHover :: Maybe Hover -> Session [Expect] -> Session ()
  checkHover hover expectations = traverse_ check =<< expectations where

    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (HoverContents MarkupContent{_value = standardizeQuotes -> msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange  expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

  checkHoverRange :: Range -> Maybe Range -> T.Text -> Session ()
  checkHoverRange expectedRange rangeInHover msg =
    let
      lineCol = extractLineColFromHoverMsg msg
      -- looks like hovers use 1-based numbering while definitions use 0-based
      -- turns out that they are stored 1-based in RealSrcLoc by GHC itself.
      adjust Position{_line = l, _character = c} =
        Position{_line = l + 1, _character = c + 1}
    in
    case map (read . T.unpack) lineCol of
      [l,c] -> liftIO $ adjust (_start expectedRange) @=? Position l c
      _     -> liftIO $ assertFailure $
        "expected: " <> show ("[...]" <> sourceFileName <> ":<LINE>:<COL>**[...]", Just expectedRange) <>
        "\n but got: " <> show (msg, rangeInHover)


data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

-}