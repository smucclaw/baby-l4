{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module ToDA2 where

import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import Data.Maybe (fromJust, catMaybes)
import Control.Monad (unless)

createDSyaml :: (Show ct) => Program ct Tp -> IO ()
createDSyaml p = putDoc $ PP.line <> showDS p


-- "printing" structure
class DSYaml x where
  showDS :: x -> Doc ann
  showDSlist :: [x] -> Doc ann
  showDSlist = vsep . map showDS

instance DSYaml (Program ct Tp) where
  showDS Program { lexiconOfProgram,classDeclsOfProgram,globalsOfProgram,rulesOfProgram,assertionsOfProgram} =
    vsep [ pretty "rules: "
         , pretty "query: "
         , pretty "data: "      , showDSlist classDeclsOfProgram
         , pretty "terms: "
         ]

instance DSYaml (ClassDecl ct) where
  showDS ClassDecl { nameOfClassDecl, defOfClassDecl } = indent' $ pretty "-" <+> showDS nameOfClassDecl
  showDSlist cls =
    let getCL' ClassDecl { nameOfClassDecl, defOfClassDecl} = (\(ClsNm x) -> x) nameOfClassDecl
        getCL x = if getCL' x `elem` ["Object", "Boolean", "Number", "Float", "Integer", "String", "Class"] then Nothing else Just x
        cls' = catMaybes (getCL <$> cls)
    in
    vsep . map showDS $ cls'

-- ClassDecl {
--              nameOfClassDecl = ClsNm "Player",
--              defOfClassDecl = ClassDef {
--                                              supersOfClassDef = [ClsNm "Class",ClsNm "Object"],
--                                              fieldsOfClassDef = [FieldDecl (FldNm "throws") (ClassT (ClsNm "Sign"))]}}

-- ClassDecl {
--              nameOfClassDecl = ClsNm "Game",
--              defOfClassDecl = ClassDef {
--                                              supersOfClassDef = [ClsNm "Class",ClsNm "Object"],
--                                              fieldsOfClassDef = [
--                                                                      FieldDecl (FldNm "participants") (TupleT [ClassT (ClsNm "Player"),
--                                                                      ClassT (ClsNm "Player")]),
--                                                                      FieldDecl (FldNm "winner") (ClassT (ClsNm "Player"))
--                                                                 ]}}],


instance DSYaml ClassName where
  showDS (ClsNm x) = pretty x

instance DSYaml Tp where
  showDS tp = pretty $ case tp of
    BoolT       -> "Boolean"
    IntT        -> "Number"
    _           -> "Unsupported Type"


-- Rule "winner" [ VarDecl "a" (ClassT (ClsNm "Player")),
--                 VarDecl "g" (ClassT (ClsNm "Game")),
--                 VarDecl "r" (ClassT (ClsNm "Sign")),
--                 VarDecl "s" (ClassT (ClsNm "Sign"))
--               ]
--             (QuantifE  (ClassT (ClsNm "Boolean")) Ex "b" (ClassT (ClsNm "Player"))
--               (BinOpE (ClassT (ClsNm "Boolean")) (BBool BBand) (AppE BoolT (AppE (FunT (ClassT (ClsNm "Game")) BoolT) (VarE (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Game")) BoolT)) (GlobalVar "Participate_in")) (VarE (ClassT (ClsNm "Player")) (LocalVar "a" 4))) (VarE (ClassT (ClsNm "Game")) (LocalVar "g" 3)))
--               (BinOpE  (ClassT (ClsNm "Boolean")) (BBool BBand) (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Game")) BoolT) (VarE  (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Game")) BoolT)) (GlobalVar "Participate_in")) (VarE  (ClassT (ClsNm "Player")) (LocalVar "b" 0))) (VarE  (ClassT (ClsNm "Game")) (LocalVar "g" 3)))
--               (BinOpE  (ClassT (ClsNm "Boolean")) (BBool BBand) (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Sign")) BoolT) (VarE  (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Sign")) BoolT)) (GlobalVar "Throw")) (VarE  (ClassT (ClsNm "Player")) (LocalVar "a" 4))) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "r" 2)))
--               (BinOpE  (ClassT (ClsNm "Boolean")) (BBool BBand) (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Sign")) BoolT) (VarE  (FunT (ClassT (ClsNm "Player")) (FunT (ClassT (ClsNm "Sign")) BoolT)) (GlobalVar "Throw")) (VarE  (ClassT (ClsNm "Player")) (LocalVar "b" 0))) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "s" 1)))
--               (AppE  BoolT (AppE  (FunT (ClassT (ClsNm "Sign")) BoolT) (VarE  (FunT (ClassT (ClsNm "Sign")) (FunT (ClassT (ClsNm "Sign")) BoolT)) (GlobalVar "Beat")) (VarE  (ClassT (ClsNm "Sign")) (LocalVar "r" 2)))
--               (VarE  (ClassT (ClsNm "Sign")) (LocalVar "s" 1))))))))
--               (AppE  (ClassT (ClsNm "Player")) (LocalVar "a" 3))) (VarE  (ClassT (ClsNm "Game")) (LocalVar "g" 2))),
