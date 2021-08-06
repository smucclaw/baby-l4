module MainHelpers (readPrelude) where

import Syntax
import Annotation
import Paths_baby_l4 (getDataFileName)
import Parser (parseNewProgram)




readPrelude :: IO (NewProgram SRng)
readPrelude = do
  l4PreludeFilepath <- getDataFileName "l4/Prelude.l4"
  do
    contents <- readFile l4PreludeFilepath
    case parseNewProgram l4PreludeFilepath contents of
      Right ast -> do
        -- print ast
        return ast
      Left err -> do
        error "Parser Error in Prelude"