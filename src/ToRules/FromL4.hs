module ToRules.FromL4 where

import ToRules.Types
import ToRules.ToDecls (filterDecls, varDeclToProductionClassDecl, varDefnToProductionDefn)
import ToRules.ToRules (filterRule)
import L4.Syntax
import Data.Either (rights)

obtRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
obtRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]

mkProd :: Program (Tp ()) -> ProductionSystem 
mkProd x = ProductionSystem {
      boilerplate = ""
    , functions = map varDefnToProductionDefn gdefns
    , queries = ""
    , classDecls = map (uncurry varDeclToProductionClassDecl) gdecls
    , globals = ""
    , rules = grules 
    } where gdecls = rights $ map filterDecls $ varDeclsOfProgram x
            gdefns = varDefnsOfProgram x
            grules = rights $ map filterRule $ rulesOfProgram x

genREP :: Program (Tp ()) -> IO ()
genREP x = do
    -- let rf = Drools 
    let rf = Clara 
    print $ showForm rf $ mkProd x