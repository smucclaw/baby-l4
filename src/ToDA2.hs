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
createDSyaml p = putDoc $ PP.line <> showDS p


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
  showDS DSBlock { blkName, blkType, blkCard, blkEncodings, blkAttrs, blkUI, blkSource} =
    hang 2 $ "-" <+> vsep [ "name:" <+> pretty blkName
                          , "type:" <+> pretty blkType 
                          , "minimum/maximum/exactly:" --cardinality goes here
                          , "ask/tell/any/other:" -- ui goes here
                          , "encodings:"  , indent 2 $ "-" <+> pretty blkEncodings -- TODO : add functionality for list-encoding
                          , putAttrs blkAttrs <> putSource blkSource <> PP.line
                          ]
      where putAttrs ba = case ba of
              [Nothing] -> PP.emptyDoc 
              _  -> "attributes:" <> PP.line <> (vsep $ map showDS $ catMaybes ba) -- attributes here (one-level only)
            putSource (Just x) = "source:" <+> pretty x
            putSource Nothing = PP.emptyDoc
              

-- ClassDecl {
--   nameOfClassDecl = ClsNm "Sign"
--   defOfClassDecl = ClassDef { 
--     supersOfClassDef = [ ClsNm "Sign", ClsNm "Class, ClsNm "Object"], 
--     fieldsOfClassDef = []
--           }
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




-- globalsOfProgram =
--         [ VarDecl _ "Participate_in" ( FunT ( ClassT ( ClsNm "Player" ) ) ( FunT ( ClassT ( ClsNm "Game" ) ) BoolT ) )
--         , VarDecl _ "Win" ( FunT ( ClassT ( ClsNm "Player" ) ) ( FunT ( ClassT ( ClsNm "Game" ) ) BoolT ) )
--         , VarDecl _ "Beat" ( FunT ( ClassT ( ClsNm "Sign" ) ) ( FunT ( ClassT ( ClsNm "Sign" ) ) BoolT ) )
--         , VarDecl _ "Throw" ( FunT (ClassT ( ClsNm "Player" ) ) ( FunT ( ClassT ( ClsNm "Sign" ) ) BoolT ) )
--         , VarDecl _ "Rock" ( ClassT ( ClsNm "Sign" ) )
--         , VarDecl _ "Paper" ( ClassT ( ClsNm "Sign" ) )
--         , VarDecl _ "Scissors" ( ClassT ( ClsNm "Sign" ) )
--         ]


classDeclToBlock :: ClassDecl ct -> DSBlock ct
classDeclToBlock ClassDecl { nameOfClassDecl, defOfClassDecl } =
  DSBlock { blkName = clnm
          , blkType = "String" -- This needs to show "boolean" for types that are supported
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
          , blkType = "Object" -- showBlkType "Object" fieldtype -- This needs to show the "boolean" for types that are supported
          , blkCard = Nothing
          , blkEncodings = map toLower fldnm ++ "(X,Y)"
          , blkAttrs = [Nothing]
          , blkUI = undefined
          , blkSource = showFTname fieldtype  -- for supported types, there is no source block
          }

showBlkType :: String -> Tp -> Either String Tp
showBlkType x tp = if elem tp [BoolT, IntT] then Right tp else Left x

showFTname :: Tp -> Maybe String 
showFTname tp = case tp of 
  (ClassT (ClsNm name)) -> Just name
  _                     -> Nothing


-- type WithProgram = Reader (Program Tp)

class DSYaml x where
  showDS :: x -> Doc ann
  showDSlist :: [x] -> Doc ann
  showDSlist = vsep . map showDS


instance DSYaml (Program Tp) where
  showDS Program { lexiconOfProgram,classDeclsOfProgram,globalsOfProgram,rulesOfProgram,assertionsOfProgram} = do
    vsep [ "rules: "
         , "query: "
         , "data:"      , hang 2 $ showDSlist $ map classDeclToBlock $ reverse $ drop 7 classDeclsOfProgram
         , "terms: "
         , "options: " <> PP.line
         ]


instance DSYaml Tp where
  showDS tp = case tp of
    BoolT         -> "Boolean"
    IntT          -> "Number"
    _             -> "Unsupported Type"
