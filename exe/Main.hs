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
import ToGF.FromSCasp.ToAnswer ( nlgModels )
import ToGF.FromL4.ToQuestions ( createQuestions )
import ToGF.NormalizeSyntax
import Annotation ( SRng, LocTypeAnnot (typeAnnot) )
import Paths_baby_l4 (getDataFileName)
import Text.Pretty.Simple (pPrint, pPrintString)
import Error (printError)
import Data.Either (rights)



import ToDA2 (createDSyaml)
import Text.Pretty.Simple (pPrint)

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
        Left err -> putStrLn ("Type Error: " ++ printError err)
        Right tpAst -> do
          let tpAstNoSrc = fmap typeAnnot tpAst

          case format args of
            Fast                     ->  pPrint tpAst
            (Fgf GFOpts { gflang = gfl, showast = True } ) -> GF.nlgAST gfl tpAstNoSrc
            (Fgf GFOpts { gflang = gfl, showast = False} ) -> GF.nlg    gfl tpAstNoSrc
            Fscasp -> do createSCasp tpAstNoSrc
            Fyaml -> do createDSyaml tpAstNoSrc
                        putStrLn "---------------"
                        createQuestions tpAstNoSrc


          -- Just a test for creating natural language from s(CASP) models.
          when (testModels args) $ do
            putStrLn "\nDemo of NLG from s(CASP) models"
            let models = rights $ map parseModel tests
            nlgModels models

    Left err -> do
      putStrLn "Parser Error:"
      print err

data Format   = Fast | Fgf GFOpts | Fscasp | Fyaml
 deriving Show

--  l4 gf en          output english only
--  l4 gf en --ast    output english AND show the GF ast
--  l4 gf    --ast    output             only the GF ast (TODO?)
--  l4 gf all         output all available languages
--  l4 ast            show Haskell AST
--  l4 yaml           dump as YAML for DocAssemble purposes

data InputOpts = InputOpts
  { format   :: Format
  , testModels :: Bool
  , filepath :: FilePath
  } deriving Show

data GFOpts = GFOpts
  { gflang  :: GF.GFlang   -- perhaps this should be a list of strings
  , showast :: Bool }
  deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "gf"   (info gfSubparser gfHelper)
               <> command "ast"  (info (pure Fast) (progDesc "Show the AST in Haskell"))
               <> command "scasp" (info (pure Fscasp) (progDesc "output to sCASP for DocAssemble purposes"))
               <> command "yaml" (info (pure Fyaml) (progDesc "output to YAML for DocAssemble purposes"))
               )
            <*> switch (long "testModels" <> help "Demo of NLG from sCASP models")
            <*> argument str (metavar "Filename")
            <**> helper
        where
          gfSubparser = fmap Fgf $ GFOpts <$> 
                          subparser 
                             ( command "all" (info (pure GF.GFall) (progDesc "tell GF to output all languages"))
                            <> command "en"  (info (pure GF.GFeng) (progDesc "tell GF to output english"))
                            <> command "swe" (info (pure GF.GFswe) (progDesc "tell GF to output swedish"))
                             )
                          <*> switch (long "ast" <> help "Print GF AST to STDERR")
                        <**> helper
          gfHelper = fullDesc
                  <> header "l4 gf - specialized for natLang output"
                  <> progDesc "Prints natLang format (subcommands: en, my)"

main :: IO ()
main = do
  let optsParse' = info (optsParse) ( fullDesc
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
