{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ToSCASP where

import Control.Applicative
import Control.Monad.Writer.Class
import Data.Char (toLower, toUpper)
import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import L4.Syntax
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
query = Stash mempty mempty

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

isPred' :: Tp t -> Bool
isPred' (FunT _ t (ClassT _ BooleanC)) = True
isPred' (FunT _ t t2) = isPred' t2
isPred' _ = False

createSCasp :: Show t => Program (Tp t) -> IO ()
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

-- Blacklist version: only remove those of form Foo -> Bar -> Bool
-- removePred :: Program t -> [VarDecl t]
-- removePred = filter (not . isPred) . globalsOfProgram

-- Whitelist version: only accept facts of the form Foo : Bar
onlyFacts :: Program t -> [VarDecl t]
onlyFacts = filter isFact . globalsOfProgram
  where
    isFact :: VarDecl t -> Bool
    isFact = isFact' . tpOfVarDecl

    isFact' (ClassT _ BooleanC) = True
    isFact' (ClassT _ IntegerC) = True
    isFact' (ClassT _ _) = True
    -- isFact' (TupleT _) = _ ---- ??????
    isFact' _ = False

commaList :: [Doc ann] -> Doc ann
commaList = vsep . punctuate comma -- separator: a, b, c

-- Don't add a dot after an empty list
dotList :: [Doc ann] -> Doc ann
dotList [] = mempty
dotList xs = endDot $ vsep $ punctuate dot xs -- terminator: a. b. c.

endDot :: Doc ann -> Doc ann
endDot x = x <> dot

instance Show t => SCasp (Program (Tp t)) where
  showSC p' = let p = normalizeProg p' in
    showSClist (assertionsOfProgram p) -- These become queries
      <> showSClist (onlyFacts p)      -- These become facts
      <> showSClist (rulesOfProgram p >>= normaliseConditionsAndConclusions >>= normalizeQuantif) -- These become rules and facts
  showSingle = unstash . showSC


instance Show t => SCasp (Rule (Tp t)) where
  showSC (Rule _ _ _ _ (ValE _ (BoolV True)) thenExp) = fact $ endDot $ showSingle thenExp
  showSC (Rule _ _ _ _ (ValE _ (BoolV False)) thenExp) = fact $ endDot $ showSingle (negateExpr thenExp)
  showSC r = rule $ showSingle r
  showSingle (Rule _ rulename _ vardecls ifExp thenExp) =
      vsep
        [ showSingle thenExp <+> pretty ":-",
          endDot $ indent' $ commaList (map showSingle vardecls ++ [showSingle ifExp]),
          PP.line
        ]

-- Only assertions like `assert Beats Rock Scissors` become facts.
-- Other assertions become rules.
instance Show t => SCasp (Assertion (Tp t)) where
  showSingle (Assertion _ _ _ query) =
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

instance Show t => SCasp (Expr (Tp t)) where
  -- We don't need a case for Forall, because normalizeQuantif has taken care of it already earlier
  showSingle (Exist x exp) = vsep $ existX : suchThat
    where
      existX = mkAtom (tpOfVarDecl x) <> parens (mkVar (nameOfVarDecl x, tpOfVarDecl x))
      suchThat =
        showSingle <$> case normalizeAndExpr exp of
          ListE _ _ es -> es
          _ -> [exp]
  showSingle x = case normalizeAndExpr x of
    ValE _ v -> showSingle v
    FunApp1 f x xTp -> mkAtom f <> parens (mkVar ((nameOfQVarName .nameOfVar) x, xTp))
    FunApp2 f x xTp y yTp -> mkAtom f <> encloseSep lparen rparen comma (mkVar <$> [((nameOfQVarName .nameOfVar) x, xTp), ((nameOfQVarName .nameOfVar) y, yTp)])
    ListE _ _ es -> commaList $ map showSingle es
    QuantifE _ _ _ es -> showSingle es
    BinOpE _ _ e1 e2 -> showSingle e1 <+> showSingle e2
    UnaOpE _ unaop exp -> showSingle unaop <+> showSingle exp
    AppE _ e1 e2 -> showSingle e1 <+> showSingle e2
    FunE _ _ es -> showSingle es
    --IfThenElseE _ ifE thenE elseE -> vsep [
    --                                  showSC ifE <> comma,
    --                                showSC thenE,
    --                              showSC elseE ]
    FldAccE {} -> pretty "TODO: FldAccE"
    VarE {} -> pretty "TODO: VarE"
    x -> error $ "not handled yet: " ++ show x
--   showSC (BinOpE s t b et et5) = _
--   showSC (VarE s t v) = _
--   showSC (FunE s t p t4 et) = _
--   showSC (FldAccE s t et f) = _
--   showSC (TupleE s t l_et) = _
--   showSC (CastE s t t3 et) = _
instance SCasp UnaOp where
  showSingle (UArith u) = mempty
  showSingle (UBool UBnot) = pretty "not"

instance Arg (Var t, Tp t) where
  mkAtom (var, tp) = mkAtom tp <> pretty "_" <> mkAtom var
  mkVar (var@(GlobalVar _), tp) = mkVar var
  mkVar (var, tp) = mkVar tp <> mkVar var

instance Arg (VarName, Tp t) where
  mkAtom (var, tp) = mkAtom tp <> pretty "_" <> mkAtom (QVarName () var)
  mkVar (var, tp) = mkVar tp <> mkVar (QVarName () var)

instance Arg (QVarName t, Tp t) where
  mkAtom (var, tp) = mkAtom tp <> pretty "_" <> mkAtom var
  mkVar (var, tp) = mkVar tp <> mkVar var

instance Arg VarName where
  mkAtom varname = mkAtom (LocalVar (QVarName () varname) 0)
  mkVar varname = mkVar (LocalVar (QVarName () varname) 0)

instance Arg (QVarName t) where
  mkAtom varname = mkAtom (LocalVar varname 0)
  mkVar varname = mkVar (LocalVar varname 0)


instance Arg (Var t) where
  mkAtom var = pretty $ toLower f : irst
    where
      f : irst = nameOfQVarName (nameOfVar var)
  mkVar var@(GlobalVar _) = mkAtom var -- to handle decl Rock : Sign, should become sign(rock)
  mkVar var =
    pretty $ toUpper f : irst
    where
      f : irst = nameOfQVarName (nameOfVar var)

instance Arg (Tp t) where
  mkAtom tp = case tp of
    ClassT _ BooleanC -> pretty "bool"
    ClassT _ IntegerC -> pretty "int"
    ClassT _ (ClsNm (f : irst)) -> pretty $ toLower f : irst
    FunT _ t1 t2 -> mkAtom t1 <> pretty "->" <> mkAtom t2
    TupleT _ ts -> encloseSep lparen rparen comma $ map mkAtom ts
    _ -> pretty "unsupportedtype"
  mkVar tp = pretty $ case tp of
    ClassT _ BooleanC -> "Bool"
    ClassT _ IntegerC -> "Int"
    ClassT _ (ClsNm (f : irst)) -> toUpper f : irst
    _ -> "UnsupportedType"

instance SCasp Val where
  showSingle (BoolV b) = pretty "Bool"
  showSingle (IntV i) = pretty "Int"
  showSingle (FloatV f) = pretty "Float"
  showSingle (StringV l_c) = pretty l_c
  -- showSingle (RecordV c l_p_fv) = pretty "unsupported, sorry"
  showSingle ErrV = pretty "Error"
