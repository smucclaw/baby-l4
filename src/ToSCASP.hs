{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module ToSCASP where

--( FunApp1, FunApp2, varName )
--( FunApp1, FunApp2, varName )
import Data.Char (toLower, toUpper)
import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import ToGF

createSCasp :: (Show ct) => Program ct Tp -> IO ()
createSCasp p = putDoc (showSC p <> PP.line)

indent' :: Doc ann -> Doc ann
indent' = indent 4

class Arg x where
  mkAtom, mkVar :: x -> Doc ann

class SCasp x where
  showSC :: x -> Doc ann
  showSClist :: [x] -> Doc ann
  showSClist = vsep . map showSC
instance SCasp (Program ct Tp) where
  showSC Program { lexiconOfProgram,classDeclsOfProgram,globalsOfProgram,rulesOfProgram,assertionsOfProgram} =
    vsep 
      [
        showSClist assertionsOfProgram,
        showSClist  rulesOfProgram
      ]


instance SCasp (Rule Tp) where
  showSC (Rule rulename vardecls ifExp thenExp) =
    vsep
      [ showSC thenExp <+> pretty ":-",
        showSClist vardecls,
        showSC ifExp
      ]

instance SCasp (Assertion Tp) where 
  showSC (Assertion assertExpr) =
    vsep [ showSC assertExpr ]

instance SCasp VarDecl where
  showSC (VarDecl v tp) = indent' $ mkAtom tp <> parens (mkVar (v, tp))

instance SCasp (Expr Tp) where
  showSC (ValE s t v) = showSC v
  showSC (FunApp1 f x xTp) = mkAtom f <> parens (mkVar (x, xTp))
  showSC (FunApp2 f x xTp y yTp) = mkAtom f <> encloseSep lparen rparen comma (mkVar <$> [(x, xTp), (y, yTp)])
  showSC (Exist x typ exp) = indent' $ vsep $ existX : suchThat
    where
      existX = mkAtom typ <> parens (mkVar (x, typ))
      suchThat = showSC <$> case toList exp of
        ListE _ _ _ es -> es
        _ -> [exp]
  showSC x = error $ "not handled yet: " ++ show x

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
  mkVar (var, tp) = mkVar tp <> mkVar var

instance Arg (VarName, Tp) where
  mkAtom (var, tp) = mkAtom tp <> pretty "_" <> mkAtom var
  mkVar (var, tp) = mkVar tp <> mkVar var

instance Arg VarName where
  mkAtom varname = mkAtom (GlobalVar varname)
  mkVar varname = mkVar (GlobalVar varname)

instance Arg Var where
  mkAtom var = pretty $ toLower f : irst
    where
      f : irst = varName var
  mkVar var = pretty $ toUpper f : irst
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
