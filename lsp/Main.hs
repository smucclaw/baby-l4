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

import qualified Language.LSP.Types            as J
import           Language.LSP.Diagnostics
import Data.Traversable

type Config = ()

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
  -- , notificationHandler J.STextDocumentDidOpen $ \msg -> do
  --   let doc  = msg ^. J.params . J.textDocument . J.uri
  --       fileName =  J.uriToFilePath doc
  --   liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
  --   sendDiagnostics (J.toNormalizedUri doc) (Just 0)
  ]

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.Range -> J.NormalizedUri -> Maybe Int -> LspM Config ()
sendDiagnostics range fileUri version = do
  let
    diags = [J.Diagnostic
              -- (J.Range (J.Position 0 1) (J.Position 0 5))
              range
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-hello") -- source
              "Example diagnostic message"
              Nothing -- tags
              (Just (J.List []))
            ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)


tshow :: Show a => a -> T.Text
tshow = T.pack . show

matchesPos' :: Int -> Int -> Token -> Bool
matchesPos' line col (Token (AlexPn _ l c) len _) =
  line == l -- && col == c
  && col >= c && col < c + len

elog :: (MonadIO m, Show a) => a -> m ()
elog = liftIO . hPutStrLn stderr . tshow

aposToPos :: AlexPosn -> Position
aposToPos (AlexPn _ l c) = Position (l - 1) (c - 1)

lookupToken :: Position -> TextDocumentIdentifier -> LspT () IO (Maybe Hover)
lookupToken pos@(Position l c) (TextDocumentIdentifier uri) = do
  -- print doc
  let nuri = toNormalizedUri uri
  sendDiagnostics (J.Range (J.Position 0 1) (J.Position 0 5)) nuri Nothing
  let Just loc = uriToFilePath uri
  allTokens <- liftIO $ scanFile loc
  -- print allTokens

  -- sendNotification SWindowShowMessage (ShowMessageParams MtInfo (tshow allTokens))
  -- _ <- sendNotification SWindowLogMessage $ LogMessageParams MtInfo $ tshow allTokens

  liftIO $ hPutStrLn stderr "foobar"
  elog allTokens
  -- _ <- sendNotification SWindowLogMessage $ LogMessageParams MtInfo $ tshow pos
  elog pos
  case allTokens of
    Left err -> do
      let
        AlexPn _ el ec = epos err
        errpos = aposToPos $ epos err
        diags = [J.Diagnostic
                  (J.Range errpos errpos)
                  (Just J.DsError)  -- severity
                  Nothing  -- code
                  (Just "lexer") -- source
                  (T.pack $ msg err)
                  Nothing -- tags
                  (Just (J.List []))
                ]
      publishDiagnostics 100 nuri Nothing (partitionBySource diags)

      _ <- sendNotification SWindowShowMessage $ ShowMessageParams MtError $ tshow err
      return Nothing -- TODO: Report this error as a diagnostic
      -- One problem with making the diagnostic is that alex by default shows errors as a string
      -- Which we would have to parse to be able to find where the error is
      -- Or somehow convince alex to use another type for reporting errors
    Right tokens -> do
      publishDiagnostics 100 nuri Nothing (partitionBySource [])
      let toks = find (matchesPos' (l + 1) (c + 1)) tokens
      -- sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ tshow pos <> tshow toks
      -- TODO: Extract length from the token to make a range
      elog pos
      elog toks
      for toks $ \tok -> do

        let
            ms = HoverContents $ markedUpContent "lsp-demo-simple-server" $ tshow toks
            -- ms = HoverContents $ markedUpContent "lsp-demo-simple-server" $ tshow doc
            range = Range pos pos
            -- rsp = Hover ms (Just range)
            rsp = Hover ms Nothing
        return $ rsp

main :: IO Int
main = do
  setupLogger Nothing ["lsp-demo"] minBound
  runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
    }
