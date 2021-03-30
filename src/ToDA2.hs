{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ToDA2 where

import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Control.Monad (unless)
import Control.Monad.Reader

createDSyaml :: Program Tp -> IO ()
createDSyaml p = putDoc $ PP.line <> runReader (showDS p) p

type WithProgram = Reader (Program Tp)

-- "printing" structure
class DSYaml x where
  showDS :: x -> WithProgram (Doc ann)
  showDSlist :: [x] -> WithProgram (Doc ann)
  showDSlist = fmap vsep . mapM showDS

instance DSYaml (Program Tp) where
  showDS Program { lexiconOfProgram,classDeclsOfProgram,globalsOfProgram,rulesOfProgram,assertionsOfProgram} = do
    something <- showDS $ last classDeclsOfProgram
    pure $ vsep 
      [ "rules: "
      , "query: "
      , "data:"      , hang 2 something
      , "terms: "
      ]

instance (Show ct) => DSYaml (ClassDecl ct) where
  showDS ClassDecl { nameOfClassDecl, defOfClassDecl } = do
    something <- showDS nameOfClassDecl
    somethingElse <- showDS defOfClassDecl
    pure $ hang 2 $ "-" <+> vsep
      [ "name:" <+> something
      , "attributes:"
      , indent 2 $ somethingElse ]


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
--                                                                      FieldDecl (FldNm "participants") (TupleT [ClassT (ClsNm "Player"), ClassT (ClsNm "Player")]),
--                                                                      FieldDecl (FldNm "winner") (ClassT (ClsNm "Player"))
--                                                                 ]}}],

instance (Show t) => DSYaml (ClassDef t) where
  showDS (ClassDef { supersOfClassDef, fieldsOfClassDef }) = 
    showDSlist fieldsOfClassDef

instance DSYaml (FieldDecl t) where
  showDS (FieldDecl _ (FldNm fldnm) fieldtype) = do
    something <- showDS fieldtype
    pure $ hang 2 $ "-" <+> vsep
      [ "name:" <+> pretty fldnm
      , "type:" <+> "String"
      , "origType:" <+> pretty (showFTname fieldtype)
      , "attributes:" 
      , something ]

showFTname (ClassT (ClsNm name)) = name
showFTname (TupleT (x:xs)) = showFTname x


instance DSYaml ClassName where
  showDS (ClsNm x) = pure $ pretty x

instance DSYaml Tp where
  showDS tp = case tp of
    BoolT         -> pure "Boolean"
    IntT          -> pure "Number"
    ClassT (ClsNm x)      -> do
      program <- ask
      someCDs <- showDS $ fromMaybe (error "message") $ getClassDeclByName program x
      pure ("Class" <+> pretty x <+> "whose contents are something like" <> PP.line <> indent 2 someCDs)
    TupleT (x:xs) -> do
      someDS <- showDS x
      pure ("Tuple whose first element is" <+> someDS)
    _             -> pure "Unsupported Type"

getClassDeclByName :: Program Tp -> String -> Maybe (ClassDecl Tp)
getClassDeclByName program wantedClassname = find (\cdecl -> (ClsNm wantedClassname) == nameOfClassDecl cdecl) $ classDeclsOfProgram program

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
