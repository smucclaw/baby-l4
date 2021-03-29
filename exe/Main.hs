{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Parser (parseProgram)
import Syntax (Program, ClassName, Tp (ErrT))
import Typing ( tpProgram, extractType )
import System.Environment ( getEnv )
import Options.Applicative
import qualified ToGF as GF
import System.IO ( stderr, hPutStr, hPutStrLn, hPrint )
import System.IO.Error (catchIOError)
import Control.Exception (catch, SomeException (SomeException))
import Control.Monad ( when, unless )
import Annotation ( SRng )
import Paths_baby_l4 (getDataFileName)


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

process :: InputOpts -> String -> IO ()
process args input = do
  let fpath = filepath args
      ast = parseProgram fpath input
  case ast of
    Right ast -> do
      preludeAst <- readPrelude

      let tpAst = tpProgram preludeAst ast
      let tpAstNoSrc = fmap extractType tpAst

      when (astHS args) $ do
        hPrint stderr tpAst
      when (astGF args) $ do
        GF.nlgAST (getGFL $ format args) tpAstNoSrc
      unless (astGF args) $ do
        GF.nlg (getGFL $ format args) tpAstNoSrc
    Left err -> do
      putStrLn "Parser Error:"
      print err
  where
    getGFL (Fall)    = GF.GFall
    getGFL (Fgf gfl) = gfl

data Format   = Fall | Fgf GF.GFlang deriving Show

data InputOpts = InputOpts
  { format   :: Format
  , astHS    :: Bool
  , astGF    :: Bool
  , filepath :: FilePath
  } deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "all" (info (pure Fall) (progDesc "Prints all available formats"))
               <> command "gf" (info gfSubparser gfHelper))
            <*> switch (long "astHS" <> help "Print Haskell AST to STDERR")
            <*> switch (long "astGF" <> help "Print GF AST to STDERR")
            <*> argument str (metavar "Filename")
        where
          gfSubparser = subparser ( command "all" (info (pure (Fgf GF.GFall)) (progDesc "tell GF to output all languages"))
                                 <> command "en" (info (pure (Fgf GF.GFeng))   (progDesc "tell GF to output english"))
                                 <> command "swe" (info (pure (Fgf GF.GFswe)) (progDesc "tell GF to output swedish"))
                                  )
                        <**> helper
          gfHelper = fullDesc
                  <> header "l4 gf - specialized for natLang output"
                  <> progDesc "Prints natLang format (subcommands: en, my)"


main :: IO ()
main = do
  let optsParse' = info (optsParse <**> helper) ( fullDesc
                                               <> header "mini-l4 - minimum l4? miniturised l4?")
  opts <- customExecParser (prefs showHelpOnError) optsParse'

  contents <- readFile $ filepath opts
  process opts contents


-- | to check if GF_LIB_PATH env variable is available
debugGF :: IO ()
debugGF = do
  hPutStrLn stderr "* debug"
  hPutStr stderr "- GF_LIB_PATH env variable :: "
  hPutStrLn stderr =<< catchIOError (getEnv "GF_LIB_PATH") (return . show)

-- | catch and print all exceptions
catchAll :: IO () -> IO ()
catchAll ioAction = catch ioAction (print @SomeException)