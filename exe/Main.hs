{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Parser (parseProgram)
import Syntax (Program, ClassName)
import Typing ( checkError )
import System.Environment ( getEnv )
import Options.Applicative
import qualified ToGF.FromL4.ToProp as GF
import System.IO ( stderr, hPutStr, hPutStrLn, hPrint )
import System.IO.Error (catchIOError)
import Control.Exception (catch, SomeException (SomeException))
import Control.Monad ( when, unless )
import ToSCASP (createSCasp)
import ToGF.FromSCasp.SCasp ( parseModel )
import ToGF.FromSCasp.AnswerToGF ( nlgModels )
import ToGF.FromL4.ToQuestions
import ToGF.NormalizeSyntax
import Annotation ( SRng, LocTypeAnnot (typeAnnot) )
import Paths_baby_l4 (getDataFileName)
import Text.Pretty.Simple (pPrint, pPrintString)
import Error (printError)
import Data.Either (rights)




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

      case checkError preludeAst (normalizeProg ast) of
        Left err -> putStrLn (printError err)
        Right tpAst -> do
          let tpAstNoSrc = fmap typeAnnot tpAst
          when (astHS args) $ do
            pPrint tpAst
            -- hPrint stderr tpAst
          when (astGF args) $ do
            GF.nlgAST (getGFL $ format args) tpAstNoSrc
          unless (astGF args) $ do
            GF.nlg (getGFL $ format args) tpAstNoSrc
          when (toSCASP args) $ do
          createSCasp tpAstNoSrc

--          hello tpAstNoSrc
      -- let models = rights $ map parseModel tests
      -- nlgModels models

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
  , ToSCASP  :: Bool
  } deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "all" (info (pure Fall) (progDesc "Prints all available formats"))
               <> command "gf" (info gfSubparser gfHelper))
            <*> switch (long "astHS" <> help "Print Haskell AST to STDERR")
            <*> switch (long "astGF" <> help "Print GF AST to STDERR")
            <*> switch (long "toSCASP" <> help "Print sCASP to STDOUT")
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

tests :: [String]
tests = [
  "{ win(A,RPS),  is_game(RPS),  is_participant_in(A,RPS),  is_player(A),  throw(A,rock), is_player(C),  is_participant_in(C,RPS),  throw(C,scissors),  beat(rock,scissors) }",
  "{ win(A,RPS),  is_game(RPS),  is_participant_in(A,RPS),  is_player(A),  throw(A,scissors),  is_player(C),  is_participant_in(C,RPS),  throw(C,paper),  beat(scissors,paper) }",
  "{ win(A,RPS),  is_game(RPS),  is_participant_in(A,RPS),  is_player(A),  throw(A,paper),  is_player(C),  is_participant_in(C,RPS),  throw(C,rock),  beat(paper,rock) }"
  ]
