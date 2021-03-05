module Main where

import Parser (parseProgram)
import Typing (tpProgram)
import System.Environment
import qualified ToGF as GF
import System.IO.Error (catchIOError)

process :: FilePath -> String -> IO ()
process filepath input = do
  let ast = parseProgram filepath input
  case ast of
    Right ast -> do
      print (tpProgram $ () <$ ast)
      --print ast

      -- to check if GF_LIB_PATH env variable is available
      putStrLn "* debug: GF_LIB_PATH env variable is: "
      catchIOError (getEnv "GF_LIB_PATH") (return . ("*   "++) . show)
        >>= putStrLn . ("*   "++)
      
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
