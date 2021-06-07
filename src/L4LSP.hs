{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Parser (parseProgram)
import Data.Foldable (for_)
import qualified Data.List as List
import Syntax
import Control.Lens.Extras (template)
import Data.Data (Data)
import Data.Either.Combinators (maybeToRight, mapLeft)
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

-- proposed change related to #67
-- change ReadFileErr Err to ReadFileErr Text, since ReadFileErr doesn't have location information
data LspError = ReadFileErr Err | TypeCheckerErr [Err] | ParserErr Err deriving (Eq, Show)

type Config = ()

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
      exceptHover <- runExceptT $ lookupTokenBare' pos uri
      -- sendDiagnosticsOnLeft uri exceptHover
      let mkResponseErr e = ResponseError ParseError (tshow e) Nothing
      responder (first mkResponseErr exceptHover)
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


parseVirtualOrRealFile :: Uri -> ExceptT LspError (LspM Config) (Maybe (Program LocAndTp))
parseVirtualOrRealFile uri = do
    contents <- getVirtualOrRealFile uri
    lift $ parseAndSendErrors uri contents

type Source = T.Text

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
makeDiagErr' (ReadFileErr err) = pure $ makeDiagErr "readfile" err
makeDiagErr' (ParserErr err) = pure $ makeDiagErr "parser" err
makeDiagErr' (TypeCheckerErr errs) = makeDiagErr "typechecker" <$> errs


publishError :: NormalizedUri -> LspError -> LspM Config ()
publishError uri err = publishDiagnostics 100 uri Nothing (partitionBySource $ makeDiagErr' err)


-- what to do with this?
clearError :: Source -> NormalizedUri -> LspM Config ()
clearError src uri = publishDiagnostics 100 uri Nothing (Map.singleton (Just src) mempty)

sendDiagnosticsOnLeft :: NormalizedUri -> Either LspError a -> MaybeT (LspM Config) a
-- sendDiagnosticsOnLeft uri = either (publishError uri) (const $ clearError uri)
sendDiagnosticsOnLeft uri (Left err) = MaybeT $ Nothing <$ publishError uri err
sendDiagnosticsOnLeft uri (Right result) = MaybeT $ Just result <$ clearError "No Error?" uri

readPrelude :: IO (Program SRng)
readPrelude = do
  l4PreludeFilepath <- getDataFileName "l4/Prelude.l4"
  do
    contents <- readFile l4PreludeFilepath
    case parseProgram l4PreludeFilepath contents of
      Right ast -> do
        -- print ast
        return ast
      Left err -> do
        error $ "Parser Error in Prelude: " ++ show err

errorToErrs :: Error -> LspError
errorToErrs e = TypeCheckerErr $ case e of
                  (ClassDeclsErr c) ->
                    case c of
                      DuplicateClassNamesCDE dc -> mkErrs stringOfClassName "Duplicate class name: " dc
                      UndefinedSuperclassCDE us -> mkErrs stringOfClassName "Undefined super class: " us
                      CyclicClassHierarchyCDE cc -> mkErrs stringOfClassName "Cyclic class hierarchy: " cc
                  (VarDeclsErr v) ->
                    case v of
                      DuplicateVarNamesVDE dup -> map (mkErrsVarRule "Duplicate var name: ") dup
                      UndefinedTypeVDE un      -> map (mkErrsVarRule "Undefined type var decl: ") un
                  (FieldDeclsErr f) ->
                    case f of
                      UndefinedTypeFDE uf -> mkErrs stringOfFieldName "Undefined field type: " uf
                      DuplicateFieldNamesFDE dupf ->  mkErrsField <$> dupf
                  (AssertionErr (AssertionErrAE ae)) -> getErrorCause "Assertion Error: " <$> ae
                  (RuleErr (RuleErrorRE re)) -> getErrorCause "Rule Error: " <$> re
                  -- consider using printErrorCause to gen err msgs for now

getErrorCause :: String -> (SRng, ErrorCause) -> Err
getErrorCause errTp (r, ec) = Err r (errTp ++ printErrorCause ec)

mkErr :: (b -> String) -> String -> (SRng, b) -> Err
mkErr f msg (r, n) = Err r -- get range
                        (((msg ++) . f) n) -- concatenate err msg with class/field/assertion name as extracted by fn f

mkErrs ::(b -> String) -> String -> [(SRng, b)] ->[Err]
mkErrs f msg = map (mkErr f msg)

mkErrsVarRule :: String -> (SRng, [Char]) -> Err
mkErrsVarRule msg (r, n) = Err r -- get range
                            (msg ++ n)-- concatenate err msg with var/rule name

mkErrsField :: (SRng, ClassName, [(SRng, FieldName)]) -> Err
mkErrsField (range, cls, fieldLs) = Err range ("Duplicate field names in the class: " ++ stringOfClassName cls ++ ": " ++ intercalate ", " (map fieldErrorRange fieldLs))

fieldErrorRange :: (SRng, FieldName) -> String
fieldErrorRange (range, field) = show range ++ ": " ++ stringOfFieldName field

type LocAndTp = LocTypeAnnot Tp

handleUriErrs :: J.Uri -> Either LspError FilePath
handleUriErrs uri =
  case uriToFilePath uri of
    Just loc -> Right loc
    Nothing  -> Left $ ReadFileErr $ Err (DummySRng "No valid range") "File not found"

getProg :: J.Uri -> T.Text -> Either LspError (Program SRng)
getProg uri contents = do
  x <- handleUriErrs uri
  mapLeft ParserErr $ parseProgram x (T.unpack contents)

-- TODO: #67 Handle different errors differently
--    - maybe add a top-level helper function that handles all the different types of errors
--    - helper function should preserve current behavior
--      - sends diagnostics with TypeCheckerErr / pArser Err & return Nothing
--      - with more serious errors (like from handleUriErrs/readFileErrs), return Left <error description>
--    - with no diagnostics, returns Nothing on the hover (add a new "NoHover" constructor to LspError).
-- handleLspErr :: Either LspError a -> LspM Config (Either ResponseError (Maybe a))

parseAndSendErrors :: J.Uri -> T.Text -> LspM Config (Maybe (Program LocAndTp))
parseAndSendErrors uri contents = runMaybeT $ do
  let loc = getProg uri contents
  let nuri = toNormalizedUri uri
  ast <- sendDiagnosticsOnLeft nuri loc

  preludeAst <- liftIO readPrelude -- TOOD: Handle errors

  let typedAstOrTypeError = checkError preludeAst ast
  sendDiagnosticsOnLeft nuri $ mapLeft errorToErrs typedAstOrTypeError

tshow :: Show a => a -> T.Text
tshow = T.pack . show

posInRange :: Position -> SRng -> Bool
posInRange (Position _line _col) (DummySRng _) = False
posInRange (Position line col) (RealSRng (SRng (Pos top left) (Pos bottom right))) =
  (line == top && col >= left || line > top)
  && (line == bottom && col <= right || line < bottom)

-- | Convert l4 source ranges to lsp source ranges
sRngToRange :: SRng -> Maybe Range
sRngToRange (RealSRng (SRng (Pos l1 c1) (Pos l2 c2))) = Just $ Range (Position l1 c1) (Position l2 c2)
sRngToRange (DummySRng _) = Nothing

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
  -- QN: Does this handle all possible errors with ast traversal?
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
-- TODO: add code for getting children of Rule & Assertion
getChildren (SRule r) = SExpr <$> [precondOfRule r, postcondOfRule r] -- Currently no VarDecl node to show type info, requires change in syntax
getChildren (SAssertion a) = SExpr <$> [exprOfAssertion a]

findAstAtPoint :: HasLoc t => Position -> Program t -> [SomeAstNode t]
findAstAtPoint pos = selectSmallestContaining pos [] . SProg



-- proposed change related to issue #67
-- tokensToHover :: Position -> Program LocAndTp -> IO (Maybe Hover)
tokensToHover :: Position -> Program LocAndTp -> IO Hover
tokensToHover pos ast = do
      let astNode = findAstAtPoint pos ast
      return $ tokenToHover astNode

-- proposed change related to issue #67
-- tokenToHover :: [SomeAstNode LocAndTp] -> Maybe Hover
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

astToText :: [SomeAstNode LocAndTp] -> T.Text
astToText (SMapping (Mapping _ from to):_) = "This block maps variable " <> T.pack from <> " to GrammaticalFramework WordNet definion " <> tshow to
astToText (SClassDecl (ClassDecl _ (ClsNm x) _):_) = "Declaration of new class : " <> T.pack x
astToText (SGlobalVarDecl (VarDecl _ n _):_) = "Declaration of global variable " <> T.pack n
astToText (SRule (Rule _ n _ _ _):_) = "Declaration of rule " <> T.pack n
astToText (SAssertion (Assertion _ _):_) = "Declaration of Assertion " <> "rule of no name for now"
astToText _ = "No hover info found"

lookupTokenBare' :: Position -> Uri -> ExceptT LspError (LspM Config) (Maybe Hover)
lookupTokenBare' pos uri = runMaybeT $ do
  ast <- MaybeT $ parseVirtualOrRealFile uri
  liftIO $ tokensToHover pos ast


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
