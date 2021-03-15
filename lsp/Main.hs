{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import Lexer
import Data.List (find)
import Data.Text.IO (hPutStrLn)
import qualified Data.Map as Map
import Data.SortedList
import System.IO (stderr)

import           Control.Lens hiding (Iso)
import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import           Language.LSP.Diagnostics
import           System.Log.Logger
import Data.Traversable ( for )
-- import Control.Monad.Identity (Identity(runIdentity))
import Data.Functor.Const (Const(..))
import Parser (parseProgram)
import Data.Foldable (for_)
import qualified Data.List as List
import Syntax
import Control.Lens.Extras (template)
import Data.Data (Data)
import Data.Either.Combinators (rightToMaybe)
-- import Syntax (Pos(..),SRng(..))

type Config = ()

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      elog "Hello world!"
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      rsp <- liftIO $ lookupTokenBare pos doc
      responder (Right rsp)
  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
    for_ fileName $ \fn -> do
      contents <- liftIO $ readFile fn
      parseAndSendErrors doc $ T.pack contents
  , notificationHandler J.STextDocumentDidSave $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument for: " ++ show fileName
    for_ fileName $ \fn -> do
      contents <- liftIO $ readFile fn
      parseAndSendErrors doc $ T.pack contents
  ]

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> Maybe Int -> LspM Config ()
sendDiagnostics fileUri version = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-hello") -- source
              "Example diagnostic message"
              Nothing -- tags
              (Just (J.List []))
            ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

parseAndSendErrors :: J.Uri -> T.Text -> LspM Config ()
parseAndSendErrors uri contents = do
  let Just loc = uriToFilePath uri
  let pres = parseProgram loc $ T.unpack contents
  case pres of
    Right err -> pure ()
    Left err -> do
      let
        nuri = toNormalizedUri uri
        diags = [J.Diagnostic
                  (errorRange err)
                  (Just J.DsError)  -- severity
                  Nothing  -- code
                  (Just "lexer") -- source
                  (T.pack $ msg err)
                  Nothing -- tags
                  (Just (J.List []))
                ]
      publishDiagnostics 100 nuri Nothing (partitionBySource diags)


tshow :: Show a => a -> T.Text
tshow = T.pack . show

posInRange :: Position -> SRng -> Bool
posInRange (Position line col) (SRng (Pos top left) (Pos bottom right)) =
  (line == top && col >= left || line > top)
  && (line == bottom && col <= right || line < bottom)

-- | Convert l4 source ranges to lsp source ranges
posToRange :: SRng -> Range
posToRange (SRng (Pos l1 c1) (Pos l2 c2)) = Range (Position l1 l2) (Position l2 c2)

-- | Extract the range from an alex/happy error
errorRange :: Err -> Range
errorRange = posToRange . epos

-- TODO: Use findAllExpressions and findExprAt to extract types for hover

-- TODO: Show type errors as diagnostics

findAllExpressions :: (Data ct, Data et) => Program ct et -> [Expr et]
findAllExpressions = toListOf template

-- | Find the smallest subexpression which contains the specified position
findExprAt :: J.Position -> Expr t -> Expr t
findExprAt pos expr =
  case List.find (posInRange pos . getLoc) (childExprs expr) of
    Nothing -> expr
    Just sub -> findExprAt pos sub

-- | Temporary bad debugging function.
-- Use @debugM@ instead
elog :: (MonadIO m, Show a) => a -> m ()
elog = liftIO . hPutStrLn stderr . tshow

lookupToken :: Position -> TextDocumentIdentifier -> LspT () IO (Maybe Hover)
lookupToken pos (TextDocumentIdentifier uri) = do
  -- print doc
  let nuri = toNormalizedUri uri
  sendDiagnostics nuri Nothing
  let Just loc = uriToFilePath uri
  allTokens <- liftIO $ scanFile loc
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
        diags = [J.Diagnostic
                  (errorRange err)
                  (Just J.DsError)  -- severity
                  Nothing  -- code
                  (Just "lexer") -- source
                  (T.pack $ msg err)
                  Nothing -- tags
                  (Just (J.List []))
                ]
      publishDiagnostics 100 nuri Nothing (partitionBySource diags)

      _ <- sendNotification SWindowShowMessage $ ShowMessageParams MtError $ tshow err
      return Nothing
    Right tokens -> do
      -- TODO: Clear diagnostics when they are no longer relevant
      -- TODO: The line below does nothing! See #19
      publishDiagnostics 100 nuri Nothing (partitionBySource [])
      publishDiagnostics 100 nuri Nothing (Map.singleton(Just "lexer")(Data.SortedList.toSortedList [])) --"lexer" in this case is the diagnosticsource
      
      let toks = find (posInRange pos . tokenPos) tokens
      -- sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ tshow pos <> tshow toks
      elog pos
      elog toks
      pure $ toks <&> \tok@Token {tokenKind} ->
        let
          ms = HoverContents $ markedUpContent "haskell" $ tshow tok --tokenKind
        in
          Hover ms $ Just $ posToRange $ tokenPos tok

-- | scanFile gives IO (Either Err [Token]) but for Hover we don't need error processing
-- so this function flips Either to Maybe
justReader :: FilePath -> IO (Maybe [Token])
justReader f = fmap rightToMaybe (scanFile f)

okayReader :: Maybe FilePath -> IO (Maybe [Token])
okayReader = maybe (pure Nothing) justReader

readAllTokens :: Uri -> IO (Maybe [Token])
readAllTokens = okayReader . uriToFilePath

tokensToHover :: Position -> [Token] -> Maybe Hover
tokensToHover pos tokens = do
      tok <- find (posInRange pos . tokenPos) tokens
      return $ tokenToHover tok

tokenToHover :: Token -> Hover
tokenToHover tok = Hover contents range
  where
    contents = HoverContents $ markedUpContent "haskell" $ tshow tok
    range = Just $ posToRange $ tokenPos tok

lookupTokenBare :: Position -> TextDocumentIdentifier -> IO (Maybe Hover)
lookupTokenBare pos (TextDocumentIdentifier uri) = do
  allTokens <- readAllTokens uri
  return (allTokens >>= tokensToHover pos)

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just ["lsp-hello-command"]
  }


main :: IO Int
main = do
  setupLogger Nothing ["lsp-demo"] minBound
  runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = lspOptions
    }
