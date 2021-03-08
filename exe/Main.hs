module Main where

import Parser (parseProgram)
import Syntax (Program, ClassName)
import Typing (tpProgram)
import System.Environment
import qualified ToGF as GF



readPrelude :: IO (Program (Maybe ClassName) ())
readPrelude = do
  let l4PreludeFilepath = "l4/Prelude.l4"
  do 
    contents <- readFile l4PreludeFilepath
    case parseProgram l4PreludeFilepath contents of
      Right ast -> do
        print ast
        return (() <$ ast)
      Left err -> do
        error "Parser Error in Prelude"

process :: FilePath -> String -> IO ()
process filepath input = do
  let ast = parseProgram filepath input
  case ast of
    Right ast -> do
      --print ast
      --print (() <$ ast)
      preludeAst <- readPrelude
      print (tpProgram preludeAst $ () <$ ast)
      --print ast
      GF.nlg ast
    Left err -> do
      putStrLn "Parser Error:"
      print err

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> putStrLn "Usage: core-language <input file>"
    [fname] -> do
      contents <- readFile fname
      process fname contents
