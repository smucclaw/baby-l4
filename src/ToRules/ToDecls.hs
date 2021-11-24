module ToRules.ToDecls where

import Prettyprinter ( line )
import L4.Syntax
import L4.SyntaxManipulation
import Data.Either (rights)
import ToRules.Types


data DeclError = DeclError String deriving Show

filterDecls :: (Show t) => VarDecl t -> Either DeclError (VarDecl t, [Tp t])
filterDecls dl@(VarDecl _ nm x) = case spine [] x of
    (tps, ClassT _ (ClsNm "Boolean")) -> Right (dl, tps)
    (_, _) -> Left $ DeclError $ "Not transpiled to class: " ++ show nm

varDeclToProductionClassDecl :: VarDecl t -> [Tp t] -> ProductionClassDecl
varDeclToProductionClassDecl (VarDecl _ nm _) tp = ProductionClassDecl nm (jusObj : fs)
    where jusObj = ProductionClassField "arg0" "Justification" "0"
          fs = zipWith (curry tpToProductionClassField) [1..(length tp)] tp

tpToProductionClassField :: (Int, Tp t) -> ProductionClassField
tpToProductionClassField (pos, c) = ProductionClassField ("arg" ++ show pos) (stringOfClassName . classNameOfTp $ c) (show pos)

varDefnToProductionDefn :: (Show t) => VarDefn t -> ProductionDefn
varDefnToProductionDefn (VarDefn _ nm tp f@FunE {}) = ProductionDefn nm (stringOfClassName . classNameOfTp $ retTp) (map tupelize vars) defnExp
    where (_, retTp) = spine [] tp
          (vars, bd) = decomposeFun f
          tupelize (VarDecl _ n t) = (stringOfClassName . classNameOfTp $ t, n)
          defnExp = exprToProdFuncExpr bd
varDefnToProductionDefn _ = error "FunE bodies allowed only"

exprToProdFuncExpr :: (Show t) => Expr t -> ProdFuncExpr
exprToProdFuncExpr (BinOpE _ (BCompar cOp) x y) = FEComparison cOp (exprToProdFuncExpr x) (exprToProdFuncExpr y)
exprToProdFuncExpr (BinOpE _ (BArith aOp) x y) = FEArithmetic aOp (exprToProdFuncExpr x) (exprToProdFuncExpr y)
exprToProdFuncExpr (ValE _ ErrV) = error "The compiler should have failed before transpilation occurs here."
exprToProdFuncExpr (ValE _ x) = FELiteral x
exprToProdFuncExpr ve@(VarE _ (GlobalVar {})) = FELiteral $ StringV $ nameOfQVarName . nameOfVar . varOfExprVarE $ ve
exprToProdFuncExpr ve@(VarE _ (LocalVar {}))  = FEVarExpr $ LocVar $ nameOfQVarName . nameOfVar . varOfExprVarE $ ve
exprToProdFuncExpr x = ProdFuncExprFail $ "cannot transform expression " ++ show x

astToDecls :: RuleFormat -> Program (Tp ()) -> IO ()
astToDecls rf x = do
    let decls = map filterDecls $ varDeclsOfProgram x
        defns = varDefnsOfProgram x
        gd = rights decls
        funcs = map varDefnToProductionDefn defns
        trans = map (uncurry varDeclToProductionClassDecl) gd
    mapM_ (print . (<>) line . showForm rf) funcs
    mapM_ (print . (<>) line . showForm rf) trans