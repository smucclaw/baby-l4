module ToGF where

import Syntax
import Prop -- the generated Haskell abstract syntax from the GF
import Paths_baby_l4
import System.Environment (withArgs)
import Control.Monad (forM_)
import Text.Printf (printf)

-- prettyprinting

-- import some sort of YAML library so we aren't just doing all the prettyprinting ourselves
-- import some sort of Python library, ditto

createYAML :: (Show ct, Show et) => Program ct et -> IO [String]
createYAML (Program lexicon _2 _3 _4 _5) = do
  return $ [ "---" ]

nlg :: (Show ct, Show et) => Program ct et -> IO ()
nlg prog = do
  yamlstrings <- createYAML prog
  putStrLn <$> yamlstrings
  sequence_ [ prop
    | prop <- program2prop prog
    ]

program2prop :: (Show ct, Show et) => Program ct et -> String
program2prop e = show e

-- later, convert "class" definitions to corresponding YAML/Python bitsies.
