module MainHelpers (readPrelude, getTpAst, HelperErr(..)) where

import Syntax
import Error
import Typing
import Annotation
import Paths_baby_l4 (getDataFileName)
import Lexer (Err)
import Parser (parseProgram)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except ( ExceptT(..) )
import Data.Either.Extra (mapLeft)


data HelperErr = LexErr Err | TpErr Error deriving (Eq, Show)

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
        error "Parser Error in Prelude"


getTpAst :: FilePath -> String -> ExceptT HelperErr IO (Program (LocTypeAnnot (Tp ())))
getTpAst fpath contents = do
  ast <- eitherToExceptT LexErr $ parseProgram fpath contents
  preludeAst <- liftIO readPrelude
  eitherToExceptT TpErr $ checkError preludeAst ast

eitherToExceptT :: Applicative m => (e1 -> e2) -> Either e1 a -> ExceptT e2 m a
eitherToExceptT  f = ExceptT . pure . mapLeft f
