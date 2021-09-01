{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
module ToGF.NormalizeSyntax where

import Annotation (TypeAnnot (updType))
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
      [VarDecl annot functname (FunT argtype returntype)
      | (functname, returntype) <- getFieldNmNType def]
      where argtype = ClassT clsname

    getFieldNmNType :: ClassDef t -> [(String, Tp)]
    getFieldNmNType (ClassDef  _ fields) = map getFieldNmNType' fields
    getFieldNmNType' (FieldDecl _ (FldNm name) tp) = (name, tp)

{- TODO: something like this
-- but make it actually work! Feel free to define more helper funs or other structure in other modules
normalizeProg :: Program t -> Program t
normalizeProg (Program annP mappings classdecs globals rules assert) =
  Program annP mappings classdecs (newGlobals++globals) rules assert
  where
    newGlobals = concatMap cd2vd classdecs
    cd2vd (ClassDecl annot clsname def) =
      [flipIfNecessary mappings $ VarDecl annot functname (FunT argtype returntype)
      | (functname, returntype) <- getFieldNmNType def]
      where argtype = ClassT clsname

    getFieldNmNType :: ClassDef t -> [(String, Tp)]
    getFieldNmNType (ClassDef  _ fields) = map getFieldNmNType' fields
    getFieldNmNType' (FieldDecl _ (FldNm name) tp) = (name, tp)

    flipIfNecessary :: [Mapping t] -> VarDecl t -> VarDecl t
    flipIfNecessary mappings vd = case vd of
      VarDecl t predname (FunT tp1 tp2) ->
        -- if predname is foo, and FunT is like, Game -> Player -> Bool
        -- and there is a mapping foo -> "{Player} _ {Game}"
        VarDecl t predname (FunT tp2 tp1)

        -- Otherwise, return the original
      _ -> vd

-}


-- flipIfNecessary :: [Mapping t] -> VarDecl t -> VarDecl t
-- flipIfNecessary mappings vd = case vd of

--   FunPattern name tp1 tp2 ->  [tp1,tp2]
    -- if predname is foo, and FunT is like, Game -> Player -> Bool
    -- and there is a mapping foo -> "{Player} _ {Game}"
    -- VarDecl t predname FunT tp2 tp1 where
    --   FunT tp2 tp1 = "(" ++ printTp t

-- flipIfNecessary =
--   case   mapping foo = "{Player} _ {Game}" -> mappinv

-- VarDecl x y i
-- FunT (ClassT (ClsNm x)) (FunT (ClassT (ClsNm y)) (IntT <- i)
pattern FunPattern :: String -> Tp -> Tp ->  VarDecl t
pattern FunPattern name tp1 tp2 <- VarDecl _ name (FunT tp1 tp2)

-- getMapArgs (Mapping t foo (Descr "whatever" [player, game]))
--            = [player, game]
-- getMapArgs :: Mapping t -> [String]
-- getMapArgs (Mapping t _ (Descr _ x)) = x

-- getVardeclArgs :: VarDecl t -> [String]
-- getVarDeclArgs (FunPattern _ tp1 tp2) = [tp1, tp2]

-- (VarDecl t "whatever" FunT player game)
checkNFlip :: Mapping t -> VarDecl t -> VarDecl t
checkNFlip (Mapping t _ (Descr _ x)) (FunPattern _ tp1 tp2)
  | x == [tp1, tp2] = VarDecl _ name (FunT tp1 tp2)
  | x == [tp2, tp1] = VarDecl _ name (FunT tp2 tp1)
  | otherwise = error $ "mapping doesn't exist " ++ show e

--  pattern FunPattern varname tp1 tp2 -- = VarDecl t varname pattern Arg2 tp1 tp2
-- checkFoo :: VarDecl -> VarDecl
-- checkFoo (VarDecl t varname (FunT tp1 tp2))  =

-- parseDescriptionM :: String -> Description
-- parseDescriptionM x
--   | '{' `elem` x =
--           let allWords = words $ filter (`notElem` "{}") x
--           in Descr {predOfDescription = unwords (tail (init allWords)), argsOfDescription =[head allWords, last allWords]}
--   | otherwise = Descr {predOfDescription = x, argsOfDescription = []}


-- checkMappings :: Mapping t -> Mapping t
-- checkMappings  ( Mapping t VarName Description)  = Mapping t varName (Desr {predOfDescription :: String , argsOfDescription :: [String]})

-- rearrange ::  [String] -> [String]
-- rearrange argsOfDescription = case argsOfDescription of
--   [tp1, tp2] -> [tp1, tp2]
--   [tp2, tp1] -> [tp1, tp2]