{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where


import L4.Parser (parseProgram)
import L4.Syntax (Program, ClassName)
import L4.Typing ( checkError )
--import SmtSBV (proveProgram)
import Proof (proveProgram)
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
import ToGF.FromL4.ToAnswers ( createPGFforAnswers )
import ToGF.NormalizeSyntax
import L4.Annotation ( SRng, LocTypeAnnot (typeAnnot) )
import Paths_baby_l4 (getDataFileName)
import Text.Pretty.Simple ( pPrint, pPrintString, pPrint )
import L4.Error (printError)
import Data.Either (rights)

import MainHelpers (readPrelude, getTpAst, HelperErr(..) )
import Control.Monad.Except (runExceptT)

import ToDA2 (createDSyaml)
import SimpleRules ( expSys )
import ToRules (astToRules)
import ToASP (astToASP)



process :: InputOpts -> String -> IO ()
process args input = do
  let fpath = filepath args
  errOrTpAstNoSrc <- runExceptT $ getTpAst fpath input 
  case errOrTpAstNoSrc of
    Left (LexErr err) -> do
      putStrLn "Parser Error:"
      print err
    Left (TpErr error) -> do
      putStrLn (printError error)
    Right tpAst -> do
      let tpAstNoSrc = typeAnnot <$> tpAst 
          normalAst = normalizeProg tpAstNoSrc -- Creates predicates of class fields

      case format args of
        Fasp                     ->  astToASP tpAstNoSrc
        Fast                     ->  pPrint tpAst
        (Fgf GFOpts { gflang = gfl, showast = True } ) -> GF.nlgAST gfl fpath normalAst
        (Fgf GFOpts { gflang = gfl, showast = False} ) -> GF.nlg    gfl fpath normalAst
        Fsmt -> proveProgram tpAstNoSrc
        Fscasp -> createSCasp normalAst
        Fyaml -> do createDSyaml tpAstNoSrc
                    putStrLn "---------------"
                    putStrLn "WIP: create the questions with GF. Below is the current progress. They are not used yet in the yaml."
                    createQuestions fpath normalAst
                    putStrLn "---------------"
                    createPGFforAnswers fpath normalAst
        (Fexpsys Graph) -> expSys tpAstNoSrc 
        (Fexpsys Rules) -> astToRules tpAstNoSrc


data Format   = Fasp | Fast | Faut | Fgf GFOpts | Fscasp  | Fsmt | Fyaml | Fexpsys ESOpts
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

data ESOpts = Graph | Rules deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "gf"   (info gfSubparser gfHelper)
               <> command "asp"  (info (pure Fasp) (progDesc "output to ASP / Clingo"))
               <> command "ast"  (info (pure Fast) (progDesc "Show the AST in Haskell"))
               <> command "aut"  (info (pure Faut) (progDesc "Automata-based operations"))
               <> command "scasp" (info (pure Fscasp) (progDesc "output to sCASP for DocAssemble purposes"))
               <> command "smt"   (info (pure Fsmt) (progDesc "Check assertion with SMT solver"))
               <> command "yaml" (info (pure Fyaml) (progDesc "output to YAML for DocAssemble purposes"))
               <> command "expsys" (info esSubparser esHelper)
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
          esSubparser = fmap Fexpsys $ 
            subparser 
              ( command "graph" (info (pure Graph) (progDesc "output predicate-propagation graph pdf"))
             <> command "rules" (info (pure Rules) (progDesc "output rules in rule-engine syntax"))
            --  <> command "clara"     (info (pure Clara)     (progDesc "output rules in Clara-rules syntax"))
            --  <> command "drools"    (info (pure Drools)    (progDesc "output rules in Drools .drl syntax"))
              ) 
            <**> helper
          esHelper = fullDesc
                  <> header "l4 expsys - for expert system stuff"
                  <> progDesc "Still WIP"


main :: IO ()
main = do
  let optsParse' = info optsParse ( fullDesc
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
