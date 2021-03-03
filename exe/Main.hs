module Main where

import Parser (parseProgram)
import Typing (tpProgram)
import System.Environment
import qualified ToGF as GF
import qualified ToDA as DA

-- TODO: add OptParse.Applicative to do the same as the current l4

process :: FilePath -> String -> IO ()
process filepath input = do
  let ast = parseProgram filepath input
  case ast of
    Right ast -> do
      print (tpProgram $ () <$ ast)
      --print ast
      GF.nlg ast
      DA.yaml ast
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
