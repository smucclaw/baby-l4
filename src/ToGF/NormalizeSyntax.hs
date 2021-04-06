module ToGF.NormalizeSyntax where

import Syntax


normalizeQuantif :: Rule t -> Rule t
normalizeQuantif (Rule ann nm decls ifE thenE) = 
  Rule ann nm (decls ++ newDecls) newIfE thenE
  where
    (newDecls,newIfE) = go ifE -- result of the recursion
    go (QuantifE ann Ex varnm tp expr) = (VarDecl ann varnm tp:newDs, newE)
       where (newDs, newE) = go expr
    go e = ([], e)

varName :: Var -> VarName
varName (GlobalVar n) = n
varName (LocalVar n _) = n

normalizeAnd :: Expr t -> Expr t
normalizeAnd e@(BinOpE ann (BBool BBand) e1 e2) = ListE ann AndList (go e)
  where
    go (BinOpE _ (BBool BBand) e1 e2) = go e1 ++ go e2
    go e = [e]
normalizeAnd e = e

{- TODO:
* A function Rule t -> [Rule t], which work on rules like
  if (Rich foo || Pretty foo || Smart foo)
  then Legal foo
and returns 3 new rules:
  if Rich foo
  then Legal foo

  if Pretty foo
  then Legal foo

  if Smart foo
  then Legal foo

* A function Program -> Program, which makes predicates out of class declarations

-}