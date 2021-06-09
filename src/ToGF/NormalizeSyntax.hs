{-# LANGUAGE TypeApplications #-}
module ToGF.NormalizeSyntax where

import Annotation (TypeAnnot (updType), HasDefault (defaultVal))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Syntax

-- This is for ToSCASP, where we want to put existential quantification into VarDecls.
normalizeQuantif :: HasDefault t => Rule t -> [Rule t]
normalizeQuantif (Rule ann nm decls ifE thenE) =
  Rule ann nm (decls ++ newDecls) actuallyNewIfE thenE : faRules
  where
    -- 1) Take care of existential quantification
    (newDecls, newIfE) = go ifE -- result of the recursion
    go (QuantifE ann Ex varnm tp expr) = (VarDecl (annotOfQVarName varnm) (nameOfQVarName varnm) tp : newDs, newE)
      where
        (newDs, newE) = go expr
    go e = ([], e)

    -- 2) Take care of universal quantification
    (faRules, negApp) = forallRule ifE
    -- If it was universally quantified, we made a new predicate and now we negate it
    actuallyNewIfE = fromMaybe newIfE negApp

    forallRule :: HasDefault t => Expr t -> ([Rule t], Maybe (Expr t))
    forallRule (QuantifE ann All name tp ifExp) = ([Rule ann "foo" vardecls ifExp thenExp], Just negThenExp)
      where
        vardecls = [VarDecl (annotOfQVarName name) (nameOfQVarName name) tp]
        predName = extractName ifExp
        newPred = VarE ann (LocalVar (QVarName defaultVal predName) 1) -- TODO: make it actually unique
        newArg = VarE ann (LocalVar name 0) 
        thenExp = AppE ann newPred newArg
        negThenExp = negateExpr thenExp
    forallRule _ = ([], Nothing)

-- Reverse of the previous normalizeQuantif: we want to move the vardecls into existential quantification
normalizeQuantifGF :: Rule t -> Rule t
normalizeQuantifGF r = r { varDeclsOfRule = [],
                           precondOfRule = wrapInExistential decls ifE }
  where
    ifE = precondOfRule r
    decls = varDeclsOfRule r
    wrapInExistential [] e = e
    wrapInExistential (VarDecl ann nm tp:xs) e = 
      wrapInExistential xs (QuantifE ann Ex (QVarName ann nm) tp e)
negateExpr :: Expr t -> Expr t
negateExpr e = UnaOpE (annotOfExpr e) (UBool UBnot) e

extractName :: Expr t -> String
extractName (ValE t v) = "someVal"
extractName (VarE t v) = map toLower $ (nameOfQVarName . nameOfVar) v
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

-- Nested binary ands into a single AndList
normalizeAndExpr :: Expr t -> Expr t
normalizeAndExpr e@(BinOpE ann (BBool BBand) e1 e2) = ListE ann AndList (go e)
  where
    go (BinOpE _ (BBool BBand) e1 e2) = go e1 ++ go e2
    go e = [e]
normalizeAndExpr e = e

-- Handles rules with multiple options:
-- rule <blah>
--   for foo : Foo
--   if Legal foo || Edible foo
--   then Good foo && MayBeEaten foo
-- becomes 4 new rules, with all permutations
normaliseConditionsAndConclusions :: Rule t -> [Rule t]  -- takes a rule t and returns a list of rules
normaliseConditionsAndConclusions (Rule ann nm decls ifE thenE) =
  [Rule ann nm decls condition conclusion
  | condition <- goCond ifE, conclusion <- goConc thenE]
  where
    goCond e@(BinOpE ann (BBool BBor) e1 e2) = goCond e1 ++ goCond e2 -- result of the recursion
    goCond e = [e]
    goConc e@(BinOpE ann (BBool BBand) e1 e2) = goConc e1 ++ goConc e2 -- result of the recursion
    goConc e = [e]

-- Make VarDecls out of ClassDecls
normalizeProg :: Program t -> Program t
normalizeProg (Program annP lex classdecs globals rules assert) =
  Program annP lex classdecs (newGlobals++globals) rules assert
  where
    newGlobals = concatMap cd2vd classdecs
    cd2vd (ClassDecl annot clsname def) =
      [VarDecl annot functname (FunT annot argtype returntype)    -- TODO (MS): the annotations probably do not contain the correct info
      | (functname, returntype) <- getFieldNmNType def]
      where argtype = ClassT annot clsname

    getFieldNmNType :: ClassDef t -> [(String, Tp t)]
    getFieldNmNType (ClassDef  _ fields) = map getFieldNmNType' fields
    getFieldNmNType' (FieldDecl _ (FldNm name) tp) = (name, tp)
