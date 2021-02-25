{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import Lexer
import Data.List (find)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)



handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      -- let params = ShowMessageRequestParams MtInfo "Turn on code lenses?"
      --       (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      -- _ <- sendRequest SWindowShowMessageRequest params $ \case
      --     Right (Just (MessageActionItem "Turn on")) -> do
      --       let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

      --       _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
      --         let cmd = Command "Say hello" "lsp-hello-command" Nothing
      --             rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
      --         responder (Right rsp)
      --       pure ()
      --     Right _ ->
      --       sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
      --     Left err ->
      --       sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
      elog "Hello world!"
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      rsp <- lookupToken pos doc
      -- responder (Right $ Just rsp)
      responder (Right rsp)
  ]

tshow :: Show a => a -> T.Text
tshow = T.pack . show

matchesPos' :: Int -> Int -> Token -> Bool
matchesPos' line col (Token (AlexPn _ l c) len _) =
  line == l -- && col == c
  && col >= c && col < c + len

elog :: (MonadIO m, Show a) => a -> m ()
elog = liftIO . hPutStrLn stderr . tshow

lookupToken :: Position -> TextDocumentIdentifier -> LspT () IO (Maybe Hover)
lookupToken pos@(Position l c) (TextDocumentIdentifier uri) = do
  -- print doc
  let Just loc = uriToFilePath uri
  allTokens <- liftIO $ scanFile loc
  -- print allTokens

  -- sendNotification SWindowShowMessage (ShowMessageParams MtInfo (tshow allTokens))
  -- _ <- sendNotification SWindowLogMessage $ LogMessageParams MtInfo $ tshow allTokens

  liftIO $ hPutStrLn stderr "foo"
  elog allTokens
  -- _ <- sendNotification SWindowLogMessage $ LogMessageParams MtInfo $ tshow pos
  elog pos
  case allTokens of
    Left err -> do
      _ <- sendNotification SWindowShowMessage $ ShowMessageParams MtError $ T.pack err
      return Nothing -- TODO: Report this error as a diagnostic
      -- One problem with making the diagnostic is that alex by default shows errors as a string
      -- Which we would have to parse to be able to find where the error is
      -- Or somehow convince alex to use another type for reporting errors
    Right tokens -> do
      let toks = find (matchesPos' (l + 1) (c + 1)) tokens
      -- sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ tshow pos <> tshow toks
      -- TODO: Extract length from the token to make a range
      elog pos
      elog toks
      let
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" $ tshow toks
          -- ms = HoverContents $ markedUpContent "lsp-demo-simple-server" $ tshow doc
          range = Range pos pos
          -- rsp = Hover ms (Just range)
          rsp = Hover ms Nothing
      return $ Just rsp

main :: IO Int
main = do
  setupLogger Nothing ["reactor"] minBound
  runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
    }
