{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
module L4LSP where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import Lexer
import Data.Text.IO (hPutStrLn)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import System.IO (stderr, FilePath)

import           Control.Lens hiding (Iso)
import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import           Language.LSP.Diagnostics
import           System.Log.Logger
-- import Control.Monad.Identity (Identity(runIdentity))
-- import Control.Monad.Identity (Identity(runIdentity))
import Parser (parseProgram, parseNewProgram)
import Data.Foldable (for_)
import qualified Data.List as List
import Syntax
import Control.Lens.Extras (template)
import Data.Data (Data)
import Data.Either.Combinators (maybeToRight, mapLeft, mapRight)
import Control.Monad.Trans.Except (except)
import Control.Monad.Except
-- import Syntax (Pos(..),SRng(..))

import Annotation
import Language.LSP.VFS
import qualified Data.Rope.UTF16 as Rope
import Data.Bifunctor (first)
import Paths_baby_l4 (getDataFileName)
import Typing (checkError)
import Error
import Data.List (intercalate)
import Control.Monad.Trans.Maybe (MaybeT(..))

type Config = ()

type Source = T.Text
type LocAndTp = LocTypeAnnot (Tp ())
type ResErrOrAst = Either ResponseError (Maybe (Program LocAndTp))
data LspError = ReadFileErr Source | TypeCheckerErr [Err] | ParserErr Err deriving (Eq, Show)

tshow :: Show a => a -> T.Text
tshow = T.pack . show


----------------------------------------------------------------------
-- LSP Handlers
----------------------------------------------------------------------
handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      liftIO $ debugM "initialize.handle" "Hello World!"
      liftIO $ debugM "initialize.handle" "Initialization successful!"
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      let (TextDocumentIdentifier uri) = doc
      let fileName =  J.uriToFilePath uri
      liftIO $ debugM "reactor.handle" $ "Processing HoverRequest for: " ++ show fileName
      exceptHover <- lookupTokenBare' pos uri
      let nuri = toNormalizedUri uri
      errOrResponse <- handleLspErr nuri exceptHover
      responder errOrResponse
  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
    for_ fileName $ \fn -> do
      contents <- liftIO $ readFile fn
      -- TODO: Handle Left (not a req, so res is ignored & errors are ignored)
      parseAndSendErrors doc $ T.pack contents
  , notificationHandler J.STextDocumentDidSave $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument for: " ++ show fileName
    for_ fileName $ \fn -> do
      contents <- liftIO $ readFile fn
      parseAndSendErrors doc $ T.pack contents
  , notificationHandler J.STextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument
                    . J.uri
                    . to J.toNormalizedUri
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
    mdoc <- getVirtualFile doc
    case mdoc of
      Just (VirtualFile _version str vf) -> do
        liftIO $ debugM "reactor.handle" $ "Found the virtual file: " ++ show str  ++ ", " ++ show vf
        _ <- parseAndSendErrors (fromNormalizedUri doc) (Rope.toText vf)
        pure ()
      Nothing -> do
        liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc
  ]

getVirtualOrRealFile :: Uri -> ExceptT LspError (LspM Config) T.Text
getVirtualOrRealFile uri = do
    mdoc <- lift . getVirtualFile $ J.toNormalizedUri uri
    case mdoc of
      Just (VirtualFile _version fileVersion vf) -> do
        liftIO $ debugM "reactor.handle" $ "Found the virtual file: " ++ show fileVersion ++ ", " ++ show vf
        pure $ Rope.toText vf
      Nothing -> do
        liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show uri
        filename <- ExceptT $ pure $ handleUriErrs uri
        -- TODO: Catch errors thrown by readFile
        liftIO $ TIO.readFile filename




----------------------------------------------------------------------
-- Error Handling utilities
----------------------------------------------------------------------
makeDiagErr :: Source -> Err -> Diagnostic
makeDiagErr source err =
  J.Diagnostic
    (errorRange err)
    (Just J.DsError)  -- severity
    Nothing  -- code
    (Just source) -- source
    (T.pack $ msg err)
    Nothing -- tags
    (Just (J.List []))

makeDiagErr' :: LspError -> [Diagnostic]
makeDiagErr' (ParserErr err) = pure $ makeDiagErr "parser" err
makeDiagErr' (TypeCheckerErr errs) = makeDiagErr "typechecker" <$> errs

publishError :: NormalizedUri -> LspError -> LspM Config ()
publishError uri err = publishDiagnostics 100 uri Nothing (partitionBySource $ makeDiagErr' err)

clearError :: Source -> NormalizedUri -> LspM Config ()
clearError src uri = publishDiagnostics 100 uri Nothing (Map.singleton (Just src) mempty)

-- This one doesn't seem to be used anymore
-- publishResponseError :: NormalizedUri -> LspM Config ()
-- publishResponseError = pure $ sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtError "readFileErr")

sendDiagnosticsOnLeft :: NormalizedUri -> Either LspError a -> MaybeT (LspM Config) a
sendDiagnosticsOnLeft uri (Left err) = MaybeT $ Nothing <$ publishError uri err
sendDiagnosticsOnLeft uri (Right result) = MaybeT $ Just result <$ clearError "No Error?" uri

errorToErrs :: Error -> LspError
errorToErrs e = TypeCheckerErr $ case e of
                  (ErrorCauseErr ecs) -> getErrorCause "" =<< ecs
                  other -> error $ "Unknown error: " ++ show other

getErrorCause :: String -> ErrorCause -> [Err]
getErrorCause errTp ec = [Err loc (errTp ++ printErrorCause ec) | loc <- getErrLocation ec]

-- TODO: #98 Replace this temporary workaround when AST is updated with positional information.
getErrLocation :: ErrorCause -> [SRng]
getErrLocation (UndefinedClassInType r _) = [r]
getErrLocation (UndeclaredVariable r _) = [r]
getErrLocation (IllTypedSubExpr srs _ _) = srs
getErrLocation (IncompatibleTp srs _) = srs
getErrLocation (NonScalarExpr srs _) = srs
getErrLocation (NonFunctionTp srs _) = srs
getErrLocation (CastIncompatible srs _ _) = srs
getErrLocation (IncompatiblePattern r) = [r]
getErrLocation (UnknownFieldName r _ _) = [r]
getErrLocation (AccessToNonObjectType r) = [r]
getErrLocation (DuplicateClassNamesCDEErr locNames) = fst <$> locNames
getErrLocation (UndefinedSuperclassCDEErr locNames) = fst <$> locNames
getErrLocation (CyclicClassHierarchyCDEErr locNames) = fst <$> locNames
getErrLocation (DuplicateFieldNamesFDEErr x) =  (\(sr, _cn, _x2) -> sr) <$> x
getErrLocation (UndefinedTypeFDEErr locNames) = fst <$> locNames
getErrLocation (DuplicateVarNamesVDEErr locNames) = fst <$> locNames
getErrLocation (UndefinedTypeVDEErr locNames) = fst <$> locNames

mkErr :: (b -> String) -> String -> (SRng, b) -> Err
mkErr f msg (r, n) = Err r -- get range
                        (((msg ++) . f) n) -- concatenate err msg with class/field/assertion name as extracted by fn f

mkErrs ::(b -> String) -> String -> [(SRng, b)] ->[Err]
mkErrs f msg = map (mkErr f msg)

mkErrsVarRule :: String -> (SRng, String) -> Err
mkErrsVarRule msg (r, n) = Err r -- get range
                            (msg ++ n)-- concatenate err msg with var/rule name

mkErrsField :: (SRng, ClassName, [(SRng, FieldName)]) -> Err
mkErrsField (r, cls, fieldLs) = Err r ("Duplicate field names in the class: " ++ stringOfClassName cls ++ ": " ++ intercalate ", " (map fieldErrorRange fieldLs))

fieldErrorRange :: (SRng, FieldName) -> String
fieldErrorRange (range, field) = show range ++ ": " ++ stringOfFieldName field

handleUriErrs :: J.Uri -> Either LspError FilePath
handleUriErrs uri =
  case uriToFilePath uri of
    Just loc -> Right loc
    Nothing  -> Left $ ReadFileErr $ "Unable to parse uri: " <> tshow (getUri uri)

handleLspErr :: NormalizedUri -> Either LspError a -> LspM Config (Either ResponseError (Maybe a))
handleLspErr _ (Left (ReadFileErr _)) = pure $ Left (ResponseError InvalidRequest "readFileErr" Nothing)
handleLspErr nuri (Left err@(TypeCheckerErr _)) = Right Nothing <$ publishError nuri err
handleLspErr nuri (Left err@(ParserErr _)) = Right Nothing <$ publishError nuri err
handleLspErr nuri (Right a) = Right (Just a) <$ clearError "typechecker" nuri



----------------------------------------------------------------------
-- Parsing functionality
----------------------------------------------------------------------
getProg :: J.Uri -> T.Text -> Either LspError (NewProgram SRng)
getProg uri contents = do
  x <- handleUriErrs uri
  mapLeft ParserErr $ parseNewProgram x (T.unpack contents)

readPrelude :: IO (NewProgram SRng)
readPrelude = do
  l4PreludeFilepath <- getDataFileName "l4/Prelude.l4"
  do
    contents <- readFile l4PreludeFilepath
    case parseNewProgram l4PreludeFilepath contents of
      Right ast -> do
        -- print ast
        return ast
      Left err -> do
        error $ "Parser Error in Prelude: " ++ show err

parseAndSendErrors :: J.Uri -> T.Text -> LspT Config IO ResErrOrAst
parseAndSendErrors uri contents = do
  lspErrOrAst <- runExceptT $ parseAndTypecheck uri contents
  let nuri = toNormalizedUri uri
  handleLspErr nuri lspErrOrAst

parseAndTypecheck :: MonadIO m => J.Uri -> T.Text -> ExceptT LspError m (Program LocAndTp)
parseAndTypecheck uri contents = do
  let errOrProg = getProg uri contents
  ast <- ExceptT $ pure errOrProg
  preludeAst <- liftIO readPrelude -- TODO: Handle errors
  let typedAstOrTypeError = checkError preludeAst ast
  ExceptT $ pure $ mapLeft errorToErrs (mapRight newProgramToProgram typedAstOrTypeError)

parseVirtualOrRealFile :: Uri -> ExceptT LspError (LspM Config) (Program LocAndTp)
parseVirtualOrRealFile uri = do
    contents <- getVirtualOrRealFile uri
    parseAndTypecheck uri contents





----------------------------------------------------------------------
-- Range & Pos utilities
----------------------------------------------------------------------
-- Range and Position come from lsp server & will be displayed on client-side, so should be left as 0-indexed
-- SRng and Pos are our own constructs, so should be 1-indexed; only converted on client-side
posInRange :: Position -> SRng -> Bool
posInRange (Position line col) srng = case sRngToRange srng of
  Just (Range (Position top left) (Position bottom right)) ->
     (line == top && col >= left || line > top)
     && (line == bottom && col <= right || line < bottom)
  Nothing -> False


-- | Convert 1-indexed l4 source ranges to 0-indexed lsp source ranges
sRngToRange :: SRng -> Maybe Range
sRngToRange (RealSRng srng) = Just $ realSRngToRange srng
sRngToRange (DummySRng _) = Nothing

realSRngToRange :: RealSRng -> Range
realSRngToRange (SRng p1 p2) = Range (posToPosition p1) (posToPosition p2)

posToPosition :: Pos -> Position
posToPosition (Pos l c) = Position (l-1) (c-1)

-- | Extract the range from an alex/happy error
errorRange :: Err -> Range
errorRange (Err s _)
  | Just rng <- sRngToRange s = rng
  | otherwise = Range (Position 0 0) (Position 999 0)

-- TODO: Use findAllExpressions and findExprAt to extract types for hover

-- | Use magic to find all Expressions in a program
findAllExpressions :: (Data t) => Program t -> [Expr t]
findAllExpressions = toListOf template

-- | Find the smallest subexpression which contains the specified position
findExprAt :: HasLoc t => J.Position -> Expr t -> Expr t
findExprAt pos expr =
  case List.find (posInRange pos . getLoc) (childExprs expr) of
    Nothing -> expr
    Just sub -> findExprAt pos sub

-- | Given a position and a parsed program, try to find if there is any expression at that location
findAnyExprAt :: (HasLoc t, Data t) => J.Position -> Program t -> Maybe (Expr t)
findAnyExprAt pos = List.find (posInRange pos . getLoc) . findAllExpressions



----------------------------------------------------------------------
-- Hover utilities
----------------------------------------------------------------------
data SomeAstNode t
  = SProg (Program t)
  | SExpr (Expr t)
  | SMapping (Mapping t)
  | SClassDecl (ClassDecl t)
  | SGlobalVarDecl (VarDecl t)
  | SRule (Rule t)
  | SAssertion (Assertion t)
  deriving (Show)

instance HasLoc t => HasLoc (SomeAstNode t) where
  getLoc astNode = getLoc $ getAnnot astNode

instance HasAnnot SomeAstNode where
  getAnnot (SProg pt) = getAnnot pt
  getAnnot (SExpr et) = getAnnot et
  getAnnot (SMapping et) = getAnnot et
  getAnnot (SClassDecl et) = getAnnot et
  getAnnot (SGlobalVarDecl et) = getAnnot et
  getAnnot (SRule et) = getAnnot et
  getAnnot (SAssertion et) = getAnnot et

  updateAnnot nt (SProg pt) = SProg $ updateAnnot nt pt
  updateAnnot nt (SExpr et) = SExpr $ updateAnnot nt et
  updateAnnot nt (SMapping et) = SMapping $ updateAnnot nt et
  updateAnnot nt (SClassDecl et) = SClassDecl $ updateAnnot nt et
  updateAnnot nt (SGlobalVarDecl et) = SGlobalVarDecl $ updateAnnot nt et
  updateAnnot nt (SRule et) = SRule $ updateAnnot nt et
  updateAnnot nt (SAssertion et) = SAssertion $ updateAnnot nt et

selectSmallestContaining :: HasLoc t => Position -> [SomeAstNode t] -> SomeAstNode t -> [SomeAstNode t]
selectSmallestContaining pos parents node =
  case List.find (posInRange pos . getLoc) (getChildren node) of
    Nothing -> node:parents
    Just sub -> selectSmallestContaining pos (node:parents) sub

getChildren :: SomeAstNode t -> [SomeAstNode t]
getChildren (SProg Program {lexiconOfProgram, classDeclsOfProgram, globalsOfProgram, rulesOfProgram, assertionsOfProgram }) =
  map SMapping lexiconOfProgram ++ map SClassDecl classDeclsOfProgram ++ map SGlobalVarDecl globalsOfProgram ++ map SRule rulesOfProgram ++ map SAssertion assertionsOfProgram
getChildren (SExpr et) = SExpr <$> childExprs et
getChildren (SMapping _) = []
getChildren (SClassDecl _) = []
getChildren (SGlobalVarDecl _) = []
getChildren (SRule r) = SExpr <$> [precondOfRule r, postcondOfRule r] -- Currently no VarDecl node to show type info, requires change in syntax
getChildren (SAssertion a) = SExpr <$> [exprOfAssertion a]



----------------------------------------------------------------------
-- Hover functionality
----------------------------------------------------------------------
findAstAtPoint :: HasLoc t => Position -> Program t -> [SomeAstNode t]
findAstAtPoint pos = selectSmallestContaining pos [] . SProg

tokensToHover :: Position -> Program LocAndTp -> IO Hover
tokensToHover pos ast = do
      let astNode = findAstAtPoint pos ast
      return $ tokenToHover astNode

tokenToHover :: [SomeAstNode LocAndTp] -> Hover
tokenToHover astNode = Hover contents range
  where
    astText = astToText astNode
    dbgInfo2 = tshow $ getChildren $ head astNode
    dbgInfo = case head astNode of
      ast@SProg{} -> T.take 80 $ tshow ast
      ast         -> tshow ast
    tpInfo = astToTypeInfo astNode
    txt = astText <> "\n\n" <> tpInfo <> "\n\n" <> dbgInfo <> "\n\n" <> dbgInfo2
    contents = HoverContents $ markedUpContent "haskell" txt
    annRange = getLoc $ head astNode
    range = sRngToRange annRange

astToTypeInfo :: [SomeAstNode LocAndTp] -> T.Text
astToTypeInfo (x:_) = tshow $ getType $ getAnnot x

arNameToString :: ARName -> String
arNameToString Nothing = "(anonymous)"
arNameToString (Just s) = s

astToText :: [SomeAstNode LocAndTp] -> T.Text
astToText (SMapping (Mapping _ from to):_) = "This block maps variable " <> T.pack from <> " to CNL description " <> tshow (predOfDescription to)
astToText (SClassDecl (ClassDecl _ (ClsNm x) _):_) = "Declaration of new class : " <> T.pack x
astToText (SGlobalVarDecl (VarDecl _ n _):_) = "Declaration of global variable " <> T.pack n
astToText (SRule Rule {nameOfRule = n}:_) = "Declaration of rule " <> T.pack (arNameToString n)
astToText (SAssertion Assertion {nameOfAssertion = n}:_) = "Declaration of Assertion " <> T.pack (arNameToString n)
astToText _ = "No hover info found"

lookupTokenBare' :: Position -> Uri -> LspM Config (Either LspError Hover)
lookupTokenBare' pos uri = runExceptT $ do
  ast <- parseVirtualOrRealFile uri
  liftIO $ tokensToHover pos ast



----------------------------------------------------------------------
--  Options & Main
----------------------------------------------------------------------
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
  setupLogger Nothing ["initialize.handle", "reactor.handle"] DEBUG
  runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = lspOptions
    }
