{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Parser (parseProgram)
import Syntax (Program, ClassName)
import Typing (tpProgram)
import System.Environment ( getEnv )
import Options.Applicative
import qualified ToGF as GF
import System.IO ( stderr, hPutStr, hPutStrLn )
import System.IO.Error (catchIOError)
import Control.Exception (catch, SomeException (SomeException))



readPrelude :: IO (Program (Maybe ClassName) ())
readPrelude = do
  let l4PreludeFilepath = "l4/Prelude.l4"
  do 
    contents <- readFile l4PreludeFilepath
    case parseProgram l4PreludeFilepath contents of
      Right ast -> do
        -- print ast
        return (() <$ ast)
      Left err -> do
        error "Parser Error in Prelude"

process :: FilePath -> String -> IO ()
process filepath input = do
  let ast = parseProgram filepath input
  case ast of
    Right ast -> do
      -- print ast
      preludeAst <- readPrelude
      -- print (preludeAst)
      print (tpProgram preludeAst ast)
      --GF.nlg ast
    Left err -> do
      putStrLn "Parser Error:"
      print err

data Format  = Fall | Fgf GFlang deriving Show
data GFlang  = GFeng | GFmalay deriving Show

data InputOpts = InputOpts
  { format   :: Format
  , filepath :: FilePath --Maybe FilePath
  } deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "all" (info (pure Fall) (progDesc "Prints all available formats"))
               <> command "gf" (info gfSubparser gfHelper))
            <*> argument str (metavar "Filename")
        where
          gfSubparser = subparser ( command "en" (info (pure (Fgf GFeng))   (progDesc "tell GF to output english"))
                                 <> command "my" (info (pure (Fgf GFmalay)) (progDesc "tell GF to output malay"))
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
  process (filepath opts) contents


-- | to check if GF_LIB_PATH env variable is available
debugGF :: IO ()
debugGF = do
  hPutStrLn stderr "* debug"
  hPutStr stderr "- GF_LIB_PATH env variable :: "
  hPutStrLn stderr =<< catchIOError (getEnv "GF_LIB_PATH") (return . show)

-- | catch and print all exceptions
catchAll :: IO () -> IO ()
catchAll ioAction = catch ioAction (print @SomeException)