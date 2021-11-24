module ToRules.FromL4 where

import ToRules.Types
import ToRules.ToDecls (astToDecls)
import ToRules.ToRules (astToRules)
import L4.Syntax

obtRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
obtRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]

genREP :: Program (Tp ()) -> IO ()
genREP x = do
    let rf' = Drools 
    astToDecls rf' x 
    astToRules rf' x
