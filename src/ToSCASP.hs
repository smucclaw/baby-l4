{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module ToSCASP where

import Control.Applicative
import Control.Monad.Writer.Class
import Data.Char (toLower, toUpper)
import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import ToGF.NormalizeSyntax
import ToGF.FromL4.ToProp

data Stash ann = Stash
  { rules :: Doc ann,
    facts :: Doc ann,
    queries :: Doc ann
  }

unstash :: Stash ann -> Doc ann
unstash (Stash r f q) =
  pretty "% facts" <> f <> line <>
  pretty "% rules" <> r <> line <>
  pretty "% queries " <> line <> q

rule, fact, query :: Doc ann -> Stash ann
rule rl = Stash rl mempty mempty
fact fc = Stash mempty fc mempty
query q = Stash mempty mempty q

instance Semigroup (Stash ann) where
  Stash rules1 facts1 queries1 <> Stash rules2 facts2 queries2 =
    Stash
      (rules1 <> line <> rules2)
      (facts1 <> line <> facts2)
      (queries1 <> line <> queries2)

instance Monoid (Stash ann) where
  mempty = Stash mempty mempty mempty

-- instance Functor (Stash ann) where
--   fmap f x = f x
--   f (<*>) x = f x
-- instance Functor (Stash ann) where
--   fmap f (Stash x) = Stash (f x)




isPred :: VarDecl t -> Bool
isPred = isPred' . tpOfVarDecl

isPred' :: Tp -> Bool
isPred' (FunT t BoolT) = True
isPred' (FunT t t2) = isPred' t2
isPred' _ = False

createSCasp :: Program Tp -> IO ()
createSCasp = putDoc . unstash . showSC

indent' :: Doc ann -> Doc ann
indent' = indent 4

vsep' :: [Stash ann] -> Stash ann
vsep' ss = Stash (vsep $ map rules ss) (vsep $ map facts ss) (vsep $ map queries ss)
class Arg x where
  mkAtom, mkVar :: x -> Doc ann

class SCasp x where
  showSC :: x -> Stash ann
  showSC = rule . showSingle
  showSingle :: x -> Doc ann
  showSClist :: [x] -> Stash ann
  showSClist = vsep' . map showSC

removePred :: Program t -> [VarDecl t]
removePred = filter (not . isPred) . globalsOfProgram

commaList :: [Doc ann] -> Doc ann
commaList = vsep . punctuate comma -- separator: a, b, c

-- Don't add a dot after an empty list
dotList :: [Doc ann] -> Doc ann
dotList [] = mempty
dotList xs = endDot $ vsep $ punctuate dot xs -- terminator: a. b. c.

endDot :: Doc ann -> Doc ann
endDot x = x <> dot

instance SCasp (Program Tp) where
  showSC p' = let p = normalizeProg p' in
    showSClist (assertionsOfProgram p) -- These become queries
      <> showSClist (removePred p)     -- These become facts
      <> showSClist (rulesOfProgram p >>= normalizeQuantif >>= normaliseConditionsAndConclusions) -- These become rules and facts
  showSingle = unstash . showSC


instance SCasp (Rule Tp) where
  showSC (Rule _ _ _ (ValE _ (BoolV True)) thenExp) = fact $ endDot $ showSingle thenExp
  showSC (Rule _ _ _ (ValE _ (BoolV False)) thenExp) = fact $ endDot $ showSingle (negateExpr thenExp)
  showSC r = rule $ showSingle r
  showSingle (Rule _ rulename vardecls ifExp thenExp) =
      vsep
        [ showSingle thenExp <+> pretty ":-",
          endDot $ indent' $ commaList (map showSingle vardecls ++ [showSingle ifExp]),
          PP.line
        ]

-- Only assertions like `assert Beats Rock Scissors` become facts.
-- Other assertions become rules.
instance SCasp (Assertion Tp) where
  showSingle (Assertion ann query) =
    vsep
        [ pretty "?-",
          endDot $ indent' $ commaList [showSingle query],
          PP.line
        ]
    --line <> showSingle bar <> line
  showSC = query . showSingle

instance SCasp (VarDecl t) where
  -- assume this is used only from Expr! Becomes a part of a Rule.
  showSingle (VarDecl _ v tp) = mkAtom tp <> parens (mkVar (v, tp))

  -- assume this is used only from Program! Becomes a fact.
  showSClist = fact . dotList . map decl2fact
    where decl2fact (VarDecl _ v tp) = mkAtom tp <> parens (mkAtom v)

instance SCasp (Expr Tp) where
  -- We don't need a case for Forall, because normalizeQuantif has taken care of it already earlier
  showSingle (Exist x typ exp) = vsep $ existX : suchThat
    where
      existX = mkAtom typ <> parens (mkVar (x, typ))
      suchThat =
        showSingle <$> case normalizeAndExpr exp of
          ListE _ _ es -> es
          _ -> [exp]
  showSingle x = case normalizeAndExpr x of
    ValE _ v -> showSingle v
    FunApp1 f x xTp -> mkAtom f <> parens (mkVar (x, xTp))
    FunApp2 f x xTp y yTp -> mkAtom f <> encloseSep lparen rparen comma (mkVar <$> [(x, xTp), (y, yTp)])
    ListE _ _ es -> commaList $ map showSingle es
    QuantifE _ _ _ _ es -> showSingle es
    BinOpE _ _ e1 e2 -> showSingle e1 <+> showSingle e2
    UnaOpE _ unaop exp -> showSingle unaop <+> showSingle exp
    NotDeriv ann _ e  -> showSingle $ UnaOpE ann (UBool UBneg) e
    AppE _ e1 e2 -> showSingle e1 <+> showSingle e2
    FunE _ _ _ es -> showSingle es
    --IfThenElseE _ ifE thenE elseE -> vsep [
    --                                  showSC ifE <> comma,
    --                                showSC thenE,
    --                              showSC elseE ]
    x -> error $ "not handled yet: " ++ show x
--   showSC (BinOpE s t b et et5) = _
--   showSC (VarE s t v) = _
--   showSC (FunE s t p t4 et) = _
--   showSC (FldAccE s t et f) = _
--   showSC (TupleE s t l_et) = _
--   showSC (CastE s t t3 et) = _
instance SCasp UnaOp where
  showSingle (UArith u) = mempty
  showSingle (UBool UBneg) = pretty "not"

instance Arg (Var, Tp) where
  mkAtom (var, tp) = mkAtom tp <> pretty "_" <> mkAtom var
  mkVar (var@(GlobalVar _), tp) = mkVar var
  mkVar (var, tp) = mkVar tp <> mkVar var

instance Arg (VarName, Tp) where
  mkAtom (var, tp) = mkAtom tp <> pretty "_" <> mkAtom var
  mkVar (var, tp) = mkVar tp <> mkVar var

instance Arg VarName where
  mkAtom varname = mkAtom (LocalVar varname 0)
  mkVar varname = mkVar (LocalVar varname 0)

instance Arg Var where
  mkAtom var = pretty $ toLower f : irst
    where
      f : irst = varName var
  mkVar var@(GlobalVar _) = mkAtom var -- to handle decl Rock : Sign, should become sign(rock)
  mkVar var =
    pretty $ toUpper f : irst
    where
      f : irst = varName var

instance Arg Tp where
  mkAtom tp = case tp of
    ClassT (ClsNm (f : irst)) -> pretty $ toLower f : irst
    BoolT -> pretty "bool"
    IntT -> pretty "int"
    FunT t1 t2 -> mkAtom t1 <> pretty "->" <> mkAtom t2
    TupleT ts -> encloseSep lparen rparen comma $ map mkAtom ts
    _ -> pretty "unsupportedtype"
  mkVar tp = pretty $ case tp of
    ClassT (ClsNm (f : irst)) -> toUpper f : irst
    BoolT -> "Bool"
    IntT -> "Int"
    _ -> "UnsupportedType"

instance SCasp Val where
  showSingle (BoolV b) = pretty "Bool"
  showSingle (IntV i) = pretty "Int"
  showSingle (StringV l_c) = pretty l_c
  showSingle (RecordV c l_p_fv) = pretty "unsupported, sorry"
  showSingle ErrV = pretty "Error"
