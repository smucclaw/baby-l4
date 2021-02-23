module Main where

import Parser (parseProgram)
import System.Environment
import qualified ToGF as GF

process :: String -> IO ()
process input = do
  let ast = parseProgram input
  case ast of
    Right ast -> do
      putStrLn (show ast)
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
      process contents
