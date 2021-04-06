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
import ToGF.FromL4.ToProp

data Stash ann = Stash
  { rules :: Doc ann,
    facts :: Doc ann
  }

rule :: Doc ann -> Stash ann
rule rl = Stash rl mempty

fact :: Doc ann -> Stash ann
fact fc = Stash mempty fc

instance Semigroup (Stash ann) where
  Stash rules1 facts1 <> Stash rules2 facts2 =
    Stash
      (rules1 <> line <> rules2)
      (facts1 <> line <> facts2)

instance Monoid (Stash ann) where
  mempty = Stash mempty mempty

isPred :: VarDecl t -> Bool
isPred = isPred' . tpOfVarDecl

isPred' :: Tp -> Bool
isPred' (FunT t BoolT) = True
isPred' (FunT t t2) = isPred' t2
isPred' _ = False

createSCasp :: Program Tp -> IO ()
createSCasp p = putDoc (rules $ showSC p <> rule PP.line)

indent' :: Doc ann -> Doc ann
indent' = indent 4

vsep' :: [Stash ann] -> Stash ann
vsep' ss = Stash (vsep $ map rules ss) (vsep $ map facts ss)

punctuateBoth :: Doc ann -> [Stash ann] -> Stash ann
punctuateBoth comma ss = Stash (vsep $ punctuate comma $ map rules ss) (vsep $ punctuate comma $ map facts ss)

punctuateFacts, punctuateRules :: Doc ann -> [Stash ann] -> Stash ann
punctuateFacts comma ss = Stash (vsep $ map rules ss) (vsep $ punctuate comma $ map facts ss)
punctuateRules comma ss = Stash (vsep $ punctuate comma $ map rules ss) (vsep $ map facts ss)

class Arg x where
  mkAtom, mkVar :: x -> Doc ann

class SCasp x where
  showSC :: x -> Stash ann
  showSC = rule . showRule
  showRule, showFact :: x -> Doc ann
  showSClist :: [x] -> Stash ann
  showSClist = vsep' . map showSC
  showRulelist :: [x] -> Doc ann
  showRulelist = vsep . map showRule

showSCcommalist :: (SCasp a) => [a] -> Doc ann
showSCcommalist = commaList . map showRule

showSCdotlist :: (SCasp a) => [a] -> Doc ann
showSCdotlist = dotList . map showRule

onlyPred :: Program t -> [VarDecl t]
onlyPred = filter isPred . globalsOfProgram

commaList :: [Doc ann] -> Doc ann
commaList = vsep . punctuate comma -- separator: a, b, c

-- Don't add a dot after an empty list
dotList :: [Doc ann] -> Doc ann
dotList [] = mempty
dotList xs = endDot $ vsep $ punctuate dot xs -- terminator: a. b. c.

endDot :: Doc ann -> Doc ann
endDot x = x <> dot

-- forallRule :: Expr t -> (Rule t, Doc ann)
-- forallRule (QuantifE ann All name tp ifExp) = (Rule ann "foo" vardecls ifExp thenExp, showRule thenExp)
--   where
--     vardecls = [VarDecl ann name tp]
--     newPred = VarE ann (LocalVar "SuperRandom" 0)
--     newArg = VarE ann (LocalVar name 1)
--     thenExp = AppE ann newPred newArg
-- forallRule _ = error "Trying to apply forall rule to a new expression"

instance SCasp (Program Tp) where
  showSC p =
    showSClist (assertionsOfProgram p)
      <> showSClist (onlyPred p)
      <> showSClist (normalizeQuantif <$> rulesOfProgram p)

-- Stash {
-- facts = vsep
--   [ pretty "\n% Facts",
--     showSClist $ assertionsOfProgram p,
--     showSClist $ onlyPred p ],
-- rules = [
--     pretty "\n% Rules",
--     showSClist $ map normalizeQuantif $ rulesOfProgram p
--   ]
normalizeRule :: a -> [a]
normalizeRule = (: [])

instance SCasp (Rule Tp) where
  showRule (Rule _ rulename vardecls ifExp thenExp) =
      vsep
        [ showRule thenExp <+> pretty ":-",
          endDot $ indent' $ commaList (map showRule vardecls ++ map showRule (normalizeRule ifExp)),
          PP.line
        ]

-- We don't want to state as a fact anything that is quantified:
--   `assert exists foo. Legal foo`
-- doesn't mean the same as s(CASP) expression `legal(foo)`.
-- Only assertions like `assert Beats Rock Scissors` become facts.
instance SCasp (Assertion Tp) where
  showSC (Assertion _ assertExpr) = case assertExpr of
    QuantifE {} -> mempty
    -- _ -> endDot $ showSC assertExpr

--showSC (Assertion _ assertExpr) = endDot $ showSC assertExpr

instance SCasp (VarDecl t) where
  showRule (VarDecl _ v tp) = mkAtom tp <> parens (mkVar (v, tp))
  showRulelist = dotList . map (\(VarDecl _ v tp) -> mkAtom tp <> parens (mkAtom v))

instance SCasp (Expr Tp) where
  showRule (Exist x typ exp) = vsep $ existX : suchThat
    where
      existX = mkAtom typ <> parens (mkVar (x, typ))
      suchThat =
        showRule <$> case toList exp of
          ListE _ _ es -> es
          _ -> [exp]
  -- showRule e@(Forall x typ exp) = vsep []
  --   where
  --     (newRule, newPred) = forallRule e
  showRule x = case toList x of
    ValE _ v -> showRule v
    FunApp1 f x xTp -> mkAtom f <> parens (mkVar (x, xTp))
    FunApp2 f x xTp y yTp -> mkAtom f <> encloseSep lparen rparen comma (mkVar <$> [(x, xTp), (y, yTp)])
    ListE _ _ es -> showSCcommalist es
    UnaOpE _ _ es -> showRule es
    QuantifE _ _ _ _ es -> showRule es
    Not x -> showRule x
    NotDeriv _ boo _ es -> if True then showRulelist [es] else showRule es
    --IfThenElseE _ ifE thenE elseE -> vsep [
    --                                  showSC ifE <> comma,
    --                                showSC thenE,
    --                              showSC elseE ]
    x -> error $ "not handled yet: " ++ show x -- if there's a QuantifE, move it into VarDecls

--   showSC (BinOpE s t b et et5) = _
--   showSC (VarE s t v) = _
--   showSC (UnaOpE s t u et) = _
--   showSC (IfThenElseE s t et et4 et5) = _
--   showSC (AppE s t et et4) = _
--   showSC (FunE s t p t4 et) = _
--   showSC (QuantifE s t q l_c t5 et) = _
--   showSC (FldAccE s t et f) = _
--   showSC (TupleE s t l_et) = _
--   showSC (CastE s t t3 et) = _
--   showSC (ListE s t l l_et) = _
--   showSC (NotDeriv s t b v et) = _

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
  mkAtom tp = pretty $ case tp of
    ClassT (ClsNm (f : irst)) -> toLower f : irst
    BoolT -> "bool"
    IntT -> "int"
    _ -> "unsupportedtype"
  mkVar tp = pretty $ case tp of
    ClassT (ClsNm (f : irst)) -> toUpper f : irst
    BoolT -> "Bool"
    IntT -> "Int"
    _ -> "UnsupportedType"

instance SCasp Val where
  showSC (BoolV b) = rule $  pretty "Bool"
  showSC (IntV i) = rule $  pretty "Int"
  showSC (StringV l_c) = rule $  pretty l_c
  showSC (RecordV c l_p_fv) = rule $  pretty "unsupported, sorry"
  showSC ErrV = rule $  pretty "Error"
