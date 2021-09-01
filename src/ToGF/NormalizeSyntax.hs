{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
module ToGF.NormalizeSyntax where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Syntax

-- This is for ToSCASP, where we want to put existential quantification into VarDecls.
normalizeQuantif :: Rule Tp -> [Rule Tp]
normalizeQuantif (Rule ann nm decls ifE thenE) =
  Rule ann nm (decls ++ newDecls) actuallyNewIfE thenE : faRules
  where
    -- 1) Take care of existential quantification
    (newDecls, newIfE) = go ifE -- result of the recursion
    go (QuantifE ann Ex varnm tp expr) = (VarDecl ann varnm tp : newDs, newE)
      where
        (newDs, newE) = go expr
    go e = ([], e)

    -- 2) Take care of universal quantification
    (faRules, negApp) = forallRule ifE
    -- If it was universally quantified, we made a new predicate and now we negate it
    actuallyNewIfE = fromMaybe newIfE negApp

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

-- Reverse of the previous normalizeQuantif: we want to move the vardecls into existential quantification
normalizeQuantifGF :: Rule Tp -> Rule Tp
normalizeQuantifGF r = r { varDeclsOfRule = [],
                           precondOfRule = wrapInExistential decls ifE }
  where
    ifE = precondOfRule r
    decls = varDeclsOfRule r
    wrapInExistential [] e = e
    wrapInExistential (VarDecl ann nm tp:xs) e = wrapInExistential xs (QuantifE ann Ex nm tp e)

negateExpr :: Expr t -> Expr t
negateExpr e = UnaOpE (annotOfExpr e) (UBool UBneg) e

extractName :: Expr t -> String
extractName (ValE _t (BoolV b)) = show b
extractName (ValE _t (IntV n)) = show n
extractName (ValE _t (StringV s)) = s
extractName (ValE _t (RecordV cn _)) = show cn
extractName (ValE _t ErrV) = "error"
extractName (VarE _t v) = map toLower $ varName v
extractName (UnaOpE _t u et) = show u ++ extractName et
extractName (BinOpE _t _b et et4) = extractName et ++ "_" ++ extractName et4
extractName (IfThenElseE _t et et3 et4) = extractName et ++ "_" ++ extractName et3 ++ "_" ++ extractName et4
extractName (AppE _t et et3) = extractName et ++ "_" ++ extractName et3
extractName (FunE _t _p _t3 et) = extractName et
extractName (QuantifE _t _q _lc _t4 et) = extractName et
extractName (FldAccE _t et _f) = extractName et
extractName (TupleE _t l_et) = intercalate "_" (map extractName l_et)
extractName (CastE _t _t2 et) = extractName et
extractName (ListE _t _l l_et) = intercalate "_" (map extractName l_et)
extractName (NotDeriv _ _ et) = extractName et

varName :: Var -> VarName
varName (GlobalVar n) = n
varName (LocalVar n _) = n

-- Nested binary ands into a single AndList
normalizeAndExpr :: Expr t -> Expr t
normalizeAndExpr e@(BinOpE ann (BBool BBand) e1 e2) = ListE ann AndList (go e)
  where
    go (BinOpE _ (BBool BBand) e1 e2) = go e1 ++ go e2
    go e = [e]
normalizeAndExpr e = e

-- | Handles rules with multiple options:
-- | rule <blah>
-- |  for foo : Foo
-- |  if Legal foo || Edible foo
-- |  then Good foo && MayBeEaten foo
-- | becomes 4 new rules, with all permutations
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
normalizeProg (Program annP lexicon classdecs globals rules assert) =
  Program annP lexicon classdecs (newGlobals++globals) rules assert
  where
    newGlobals = concatMap cd2vd classdecs
    cd2vd (ClassDecl annot clsname def) =
      [VarDecl annot functname (FunT argtype returntype)
      | (functname, returntype) <- getFieldNmNType def]
      where argtype = ClassT clsname

    getFieldNmNType :: ClassDef t -> [(String, Tp)]
    getFieldNmNType (ClassDef  _ fields) = map getFieldNmNType' fields
    getFieldNmNType' (FieldDecl _ (FldNm name) tp) = (name, tp)

-- | Flip the arguments of binary predicates if lexicon indicates so:
-- |    decl    win  : Game -> Player -> Bool
-- |    lexicon win -> "{Player} wins {Game}"
-- | will flip the predicate win into Player -> Game -> Bool
checkNFlip :: Mapping t -> VarDecl t -> VarDecl t
checkNFlip (Mapping _t lexName e@(Descr _ args)) vd@(Fun2 ann funName arg1 arg2)
  | lexName == funName &&
    args == [arg2, arg1] = Fun2 ann funName arg2 arg1
--  | args == [arg1, arg2] = vd
--  | otherwise = error $ "mapping doesn't exist " ++ show e
checkNFlip _ vd = vd

pattern Fun2 :: t -> VarName -> String -> String -> VarDecl t
pattern Fun2 ann name arg1 arg2 = Syntax.VarDecl ann name (Arg2 arg1 arg2)

pattern Arg0 :: String -> Tp
pattern Arg0 x = ClassT (ClsNm x)

pattern Arg1 :: String -> Tp
pattern Arg1 x = FunT (Arg0 x) BoolT

pattern Arg2 :: String -> String -> Tp
pattern Arg2 x y = FunT (Arg0 x) (Arg1 y)
