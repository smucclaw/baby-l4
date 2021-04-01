{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module ToSCASP where

import Data.Char (toLower, toUpper)
import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import ToGF.FromL4.ToProp

createSCasp :: Program Tp -> IO ()
createSCasp p = putDoc (showSC p <> PP.line)

indent' :: Doc ann -> Doc ann
indent' = indent 4

class Arg x where
  mkAtom, mkVar :: x -> Doc ann

class SCasp x where
  showSC :: x -> Doc ann
  showSClist :: [x] -> Doc ann
  showSClist = vsep . map showSC

showSCcommalist :: (SCasp a) => [a] -> Doc ann
showSCcommalist = commaList . map showSC

showSCdotlist :: (SCasp a) => [a] -> Doc ann
showSCdotlist = dotList . map showSC

commaList :: [Doc ann] -> Doc ann
commaList = vsep . punctuate comma

dotList :: [Doc ann] -> Doc ann
dotList = vsep . punctuate dot

endDot :: Doc ann -> Doc ann
endDot x = x <> dot

instance SCasp (Program Tp) where
  showSC Program { lexiconOfProgram,classDeclsOfProgram,globalsOfProgram,rulesOfProgram,assertionsOfProgram} =
    vsep
      [
        pretty "\n% Facts",
        showSClist assertionsOfProgram,
        showSClist globalsOfProgram,
        pretty "\n% Rules",
        showSClist $ map normalizeQuantif rulesOfProgram
      ]


instance SCasp (Rule Tp) where
  showSC (Rule _ rulename vardecls ifExp thenExp) =
    vsep
      [ showSC thenExp <+> pretty ":-",
        endDot $ indent' $ vsep $ punctuate comma (map showSC vardecls ++ [showSC ifExp]),
        PP.line
      ]

instance SCasp (Assertion Tp) where
  showSC (Assertion _ assertExpr) = endDot $ showSC assertExpr

instance SCasp (VarDecl t) where
  showSC (VarDecl _ v tp) = mkAtom tp <> parens (mkVar (v,tp))
  showSClist = endDot . dotList . map (\(VarDecl _ v tp) -> mkAtom tp <> parens (mkAtom v))


instance SCasp (Expr Tp) where
  showSC (Exist x typ exp) = vsep $ existX : suchThat
    where
      existX = mkAtom typ <> parens (mkVar (x, typ))
      suchThat = showSC <$> case toList exp of
        ListE _ _ es -> es
        _ -> [exp]
  showSC x = case toList x of
    ValE _ v -> showSC v
    FunApp1 f x xTp -> mkAtom f <> parens (mkVar (x, xTp))
    FunApp2 f x xTp y yTp -> mkAtom f <> encloseSep lparen rparen comma (mkVar <$> [(x, xTp), (y, yTp)])
    ListE _ _ es -> showSCcommalist es
    x -> error $ "not handled yet: " ++ show x-- if there's a QuantifE, move it into VarDecls

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
  showSC (BoolV b) = pretty "Bool"
  showSC (IntV i) = pretty "Int"
  showSC (StringV l_c) = pretty l_c
  showSC (RecordV c l_p_fv) = pretty "unsupported, sorry"
  showSC ErrV = pretty "Error"
{-
foo =
        [ Rule
            "winner" -- RuleName
            [VarDecl "a" (ClassT (ClsNm "Player")), -- for a : Player, g : Game , ... : [VarDecl]
            VarDecl "g" (ClassT (ClsNm "Game")),
            VarDecl "r" (ClassT (ClsNm "Sign")),
            VarDecl "s" (ClassT (ClsNm "Sign"))]

            (QuantifE (ClassT (ClsNm "Boolean"))    -- if exists b : Player .... : Expr t
                      Ex "b" (ClassT (ClsNm "Player"))
                          (BinOpE (ClassT (ClsNm "Boolean"))
                            (BBool BBand)
                          (AppE BoolT (AppE (FunT (ClassT (ClsNm "Game")) BoolT) (VarE (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Game")) BoolT)) (GlobalVar "Participate_in")) (VarE (ClassT (ClsNm "Player")) (LocalVar "a" 4))) (VarE (ClassT (ClsNm "Game")) (LocalVar "g" 3))) (BinOpE (ClassT (ClsNm "Boolean"))
                            (BBool BBand)
                          (AppE BoolT (AppE (FunT (ClassT (ClsNm "Game")) BoolT) (VarE (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Game")) BoolT)) (GlobalVar "Participate_in")) (VarE (ClassT (ClsNm "Player")) (LocalVar "b" 0))) (VarE (ClassT (ClsNm "Game")) (LocalVar "g" 3))) (BinOpE  (ClassT (ClsNm "Boolean"))
                            (BBool BBand)
                          (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Sign")) BoolT) (VarE  (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Sign")) BoolT)) (GlobalVar "Throw")) (VarE  (ClassT (ClsNm "Player")) (LocalVar "a" 4))) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "r" 2))) (BinOpE  (ClassT (ClsNm "Boolean"))
                            (BBool BBand)
                          (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Sign")) BoolT) (VarE  (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Sign")) BoolT)) (GlobalVar "Throw")) (VarE  (ClassT (ClsNm "Player")) (LocalVar "b" 0))) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "s" 1))) (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Sign")) BoolT) (VarE  (FunT (ClassT (ClsNm "Sign")) (FunT (ClassT (ClsNm "Sign")) BoolT)) (GlobalVar "Beat")) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "r" 2))) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "s" 1))))))))

            ( AppE  -- then Win a g : Expr t

                BoolT
                (AppE (FunT (ClassT (ClsNm "Game")) BoolT)
                (VarE (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Game")) BoolT)) (GlobalVar "Win"))
                (VarE (ClassT (ClsNm "Player")) (LocalVar "a" 3)))
                ( VarE
                    (ClassT (ClsNm "Game"))
                    (LocalVar "g" 2)
                )
            )
        ]
-}
