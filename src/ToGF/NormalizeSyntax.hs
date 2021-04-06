module ToGF.NormalizeSyntax where

import Annotation (TypeAnnot (updType))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Syntax

normalizeQuantif :: Rule Tp -> [Rule Tp]
normalizeQuantif (Rule ann nm decls ifE thenE) =
  Rule ann nm (decls ++ newDecls) actuallyNewIfE thenE : faRules
  where
    (faRules, negApp) = forallRule ifE
    -- If it was universally quantified, we made a new predicate and now we negate it
    actuallyNewIfE = fromMaybe newIfE negApp
    (newDecls, newIfE) = go ifE -- result of the recursion
    go (QuantifE ann Ex varnm tp expr) = (VarDecl ann varnm tp : newDs, newE)
      where
        (newDs, newE) = go expr
    go e = ([], e)
    forallRule :: Expr Tp -> ([Rule Tp], Maybe (Expr Tp))
    forallRule (QuantifE ann All name tp ifExp) = ([Rule ann "foo" vardecls ifExp thenExp], Just negThenExp)
      where
        vardecls = [VarDecl ann name tp]
        predName = extractName ifExp
        newPred = VarE ann (LocalVar predName 1) -- TODO: make it actually unique
        newArg = VarE tp (LocalVar name 0)
        thenExp = AppE ann newPred newArg
        negThenExp = negateExpr thenExp
    forallRule _ = ([], Nothing)

negateExpr :: Expr t -> Expr t
negateExpr e = UnaOpE (annotOfExpr e) (UBool UBneg) e

extractName :: Expr t -> String
extractName (ValE t v) = "someVal"
extractName (VarE t v) = map toLower $ varName v
extractName (UnaOpE t u et) = show u ++ extractName et
extractName (BinOpE t b et et4) = extractName et ++ "_" ++ extractName et4
extractName (IfThenElseE t et et3 et4) = extractName et ++ "_" ++ extractName et3 ++ "_" ++ extractName et4
extractName (AppE t et et3) = extractName et ++ "_" ++ extractName et3
extractName (FunE t p t3 et) = extractName et
extractName (QuantifE t q l_c t4 et) = extractName et
extractName (FldAccE t et f) = extractName et
extractName (TupleE t l_et) = intercalate "_" (map extractName l_et)
extractName (CastE t t2 et) = extractName et
extractName (ListE t l l_et) = intercalate "_" (map extractName l_et)
extractName (NotDeriv _ _ et) = extractName et

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

  if Legal foo
  then Good foo && MayBeEaten foo

and returns 2 new rules:

  if Legal foo
  then Good foo 
  
  if Legal foo
  then MayBeEaten foo

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
