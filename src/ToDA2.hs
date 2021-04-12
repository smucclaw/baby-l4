{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module ToDA2 where

import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax
import Data.List (find, intersperse)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, Maybe (Nothing))
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Applicative (liftA2)

createDSyaml :: Program Tp -> IO ()
createDSyaml p = putDoc $ PP.line <> runReader (showDS p) p


-- Perhaps instead of using Maybe, an Either would be better. 
-- With Left :: Doc ann = PP.emptyDoc, and right :: WithProgram (Doc ann) = concretized
data DSBlock t = DSBlock { blkName  :: String
                         , blkType  :: String
                         , blkCard  :: Maybe Int -- cardinality
                         , blkEncodings :: String
                         , blkAttrs :: [Maybe (DSBlock t)]
                         , blkUI    :: [Maybe t] -- ask, tell, any, another
                         , blkSource :: Maybe String
                         }


instance (Show t) => DSYaml (DSBlock t) where
  showDS DSBlock { blkName, blkType, blkCard, blkEncodings, blkAttrs, blkUI, blkSource} = do
    attrs <- showDSlist $ catMaybes blkAttrs
    pure $ hang 2 $ "-" <+> vsep [ "name:" <+> pretty blkName
                                , "type:" <+> pretty blkType <> putSource blkSource -- workaround to not print an emptyline when source is not present
                                , "minimum/maximum/exactly:" --cardinality goes here
                                , "ask/tell/any/other:" -- ui goes here
                                , docAttrs attrs blkAttrs
                                ]
      where docAttrs attrs ba = case ba of
              [Nothing] -> ""
              _  -> "attributes:" <> PP.line <> indent 2 attrs -- attributes here (one-level only)
            putSource (Just x) = PP.line <> "source:" <+> pretty x
            putSource Nothing = PP.emptyDoc
              

-- ClassDecl {
--   nameOfClassDecl = ClsNm "Class",
--   defOfClassDecl = ClassDef {
--     supersOfClassDef = [ClsNm "Object"],
--     fieldsOfClassDef = []}}], 
--
-- ClassDecl {
--   nameOfClassDecl = ClsNm "Player",
--   defOfClassDecl = ClassDef {
--     supersOfClassDef = [ClsNm "Class",ClsNm "Object"],
--     fieldsOfClassDef = [FieldDecl (FldNm "throws") (ClassT (ClsNm "Sign"))]}}

-- ClassDecl {
--   nameOfClassDecl = ClsNm "Game",
--   defOfClassDecl = ClassDef {
--     supersOfClassDef = [ClsNm "Class",ClsNm "Object"],
--     fieldsOfClassDef = [
--       FieldDecl (FldNm "participants") (TupleT [ClassT (ClsNm "Player"), ClassT (ClsNm "Player")]),
--       FieldDecl (FldNm "winner") (ClassT (ClsNm "Player"))
--     ]}}],

classDeclToBlock :: ClassDecl ct -> DSBlock ct
classDeclToBlock ClassDecl { nameOfClassDecl, defOfClassDecl } =
  DSBlock { blkName = clnm
          , blkType = "String"
          , blkCard = Nothing
          , blkEncodings = map toLower clnm ++ "(X)"
          , blkAttrs = mapAttrs $ fieldsOfClassDef defOfClassDecl 
          , blkUI = undefined
          , blkSource = Nothing
          }
  where clnm = (\(ClsNm name) -> name) nameOfClassDecl
        mapAttrs x = case x of 
          [] -> [Nothing]
          _  -> map (Just . fieldDeclToBlock) x

fieldDeclToBlock :: FieldDecl ct -> DSBlock ct
fieldDeclToBlock (FieldDecl _ (FldNm fldnm) fieldtype) =
  DSBlock { blkName = fldnm
          , blkType = "String"
          , blkCard = Nothing
          , blkEncodings = map toLower fldnm ++ "(Y)"
          , blkAttrs = [Nothing]
          , blkUI = undefined
          , blkSource = Just $ showFTname fieldtype 
          }

showFTname (ClassT (ClsNm name)) = name
showFTname (TupleT (x:xs)) = showFTname x


type WithProgram = Reader (Program Tp)


class DSYaml x where
  showDS :: x -> WithProgram (Doc ann)
  showDSlist :: [x] -> WithProgram (Doc ann)
  showDSlist = fmap vsep . mapM showDS


instance DSYaml (Program Tp) where
  showDS Program { lexiconOfProgram,classDeclsOfProgram,globalsOfProgram,rulesOfProgram,assertionsOfProgram} = do
    something <- showDSlist $ map classDeclToBlock $ reverse $ drop 7 classDeclsOfProgram
    pure $ vsep
      [ "rules: "
      , "query: "
      , "data:"      , hang 2 something
      , "terms: "
      , "options: " <> PP.line
      ]


instance DSYaml Tp where
  showDS tp = case tp of
    BoolT         -> pure "Boolean"
    IntT          -> pure "Number"
    _             -> pure "Unsupported Type"
