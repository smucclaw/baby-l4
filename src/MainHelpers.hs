module MainHelpers (readPrelude) where

import Syntax
import Annotation
import Paths_baby_l4 (getDataFileName)
import Parser (parseProgram)




readPrelude :: IO (Program SRng)
readPrelude = do
  l3PreludeFilepath <- getDataFileName "l4/Prelude.l4"
  do
    contents <- readFile l3PreludeFilepath
    case parseProgram l3PreludeFilepath contents of
      Right ast -> do
        -- print ast
        return ast
      Left err -> do
        error "Parser Error in Prelude"