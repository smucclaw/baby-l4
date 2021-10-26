{-# LANGUAGE NamedFieldPuns #-}
module ToRules.ToDecls where

import Prettyprinter
import Syntax
import SyntaxManipulation
import Annotations
import Data.Either (rights)
import ToRules.Types


data DeclError = DeclError String deriving Show

filterDecls :: (Show t) => VarDecl t -> Either DeclError (VarDecl t, [Tp t])
filterDecls dl@(VarDecl _ nm x) = case spine [] x of
    (tps, ClassT _ (ClsNm "Boolean")) -> Right (dl, tps)
    (_, _) -> Left $ DeclError $ "Not transpiled to class: " ++ show nm

varDeclToProductionClassDecl :: VarDecl t -> [Tp t] -> ProductionClassDecl
varDeclToProductionClassDecl (VarDecl _ nm _) tp = ProductionClassDecl nm fs
    where fs = map tpToProductionClassField $ zip [0..(length tp- 1)] tp

tpToProductionClassField :: (Int, Tp t) -> ProductionClassField
tpToProductionClassField (pos, c) = ProductionClassField ("arg" ++ show pos) (yieldNativeTp . stringOfClassName . classNameOfTp $ c)
    where yieldNativeTp x = if x `elem` ["Integer", "Boolean", "Float"] then x else "String"

astToDecls :: RuleFormat -> Program (Tp ()) -> IO ()
astToDecls rf x = do
    let decls = map filterDecls $ globalsOfProgram x
        gd = rights decls
        trans = map (uncurry varDeclToProductionClassDecl) gd
    mapM_ (print . (<>) line . showForm rf) trans