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
createDSyaml p = putDoc $ showDS p


-- Perhaps instead of using Maybe, an Either would be better. 
-- With Left :: Doc ann = PP.emptyDoc, and right :: WithProgram (Doc ann) = concretized
data DSBlock t = DSBlock { blkName  :: String
                         , blkType  :: Either String Tp 
                         , blkCard  :: Maybe Int -- cardinality
                         , blkEncodings :: String
                         , blkAttrs :: [Maybe (DSBlock t)]
                         , blkUI    :: [Maybe t] -- ask, tell, any, another
                         , blkSource :: Maybe String
                         }


instance (Show t) => DSYaml (DSBlock t) where
  showDS DSBlock { blkName, blkType, blkCard, blkEncodings, blkAttrs, blkUI, blkSource} =
    hang 2 $ "-" <+> vsep [ "name:" <+> pretty blkName
                          , "type:" <+> either pretty showDS blkType 
                          , "minimum: 0 # Change cardinality accordingly" --cardinality goes here
                          -- , "ask/tell/any/other:" -- ui goes here
                          , "encodings:"  , indent 2 $ "-" <+> pretty blkEncodings -- TODO : add functionality for list-encoding
                          , putSource blkSource <> putAttrs blkAttrs <>  PP.line
                          ]
      where putAttrs ba = case ba of
              [Nothing] -> PP.emptyDoc 
              _  -> "attributes:" <> PP.line <> indent 2 (showDSlist $ catMaybes ba) -- attributes here (one-level only)
            putSource (Just x) = "source:" <+> pretty x
            putSource Nothing = PP.emptyDoc
              

-- ClassDecl { 
--     annotOfClassDecl = _           
--   , nameOfClassDecl = ClsNm "Sign"
--   , defOfClassDecl = ClassDef { supersOfClassDef = [ ClsNm "Sign"
--                                                    , ClsNm "Class"
--                                                    , ClsNm "Object" ]
--                               , fieldsOfClassDef = [ FieldDecl { annotOfFieldDecl = _
--                                                                , nameOfFieldDecl = FldNm "beat"
--                                                                , tpOfFieldDecl = FunT
--                                                                    ( ClassT ( ClsNm "Sign" ) ) BoolT }]} }
--
-- ClassDecl { annotOfClassDecl = _
--           , nameOfClassDecl = ClsNm "Player"
--           , defOfClassDecl = ClassDef { supersOfClassDef = [ ClsNm "Player"
--                                                            , ClsNm "Class"
--                                                            , ClsNm "Object" ]
--                                       , fieldsOfClassDef = [ FieldDecl { annotOfFieldDecl = _ 
--                                                                        , nameOfFieldDecl = FldNm "throw"
--                                                                        , tpOfFieldDecl = FunT ( ClassT ( ClsNm "Sign" ) ) BoolT } ] } }

-- ClassDecl { annotOfClassDecl = _
--           , nameOfClassDecl = ClsNm "Game"
--           , defOfClassDecl = ClassDef { supersOfClassDef = [ ClsNm "Game"
--                                                            , ClsNm "Class"
--                                                            , ClsNm "Object" ]
--                                       , fieldsOfClassDef = [ FieldDecl { annotOfFieldDecl = _
--                                                                        , nameOfFieldDecl = FldNm "participate_in"
--                                                                        , tpOfFieldDecl = FunT ( ClassT ( ClsNm "Player" ) ) BoolT
--                                                                        }
--                                                            , FieldDecl { annotOfFieldDecl = _
--                                                                        , nameOfFieldDecl = FldNm "win"
--                                                                        , tpOfFieldDecl = FunT ( ClassT ( ClsNm "Player" ) ) BoolT
--                                                                        }
--                                                            ] } } ]



classDeclToBlock :: ClassDecl ct -> DSBlock ct
classDeclToBlock ClassDecl { nameOfClassDecl, defOfClassDecl } =
  DSBlock { blkName = lowercase clnm
          , blkType = Left "String" -- This needs to show "boolean" for types that are supported
          , blkCard = Nothing
          , blkEncodings = lowercase clnm ++ "(X)"
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
  DSBlock { blkName = lowercase fldnm
          , blkType = eitherTp "Object" fieldtype fieldtype -- This needs to show the "boolean" for types that are supported
          , blkCard = Nothing
          , blkEncodings = (lowercase fldnm ++) $ either id id $ eitherTp "(X,Y)" "(Y)" fieldtype
          , blkAttrs = [Nothing]
          , blkUI = undefined
          , blkSource = showFTname fieldtype  -- for supported types, there is no source block
          }

eitherTp :: String -> a -> Tp -> Either String a
eitherTp x1 x2 tp = if elem tp [BoolT, IntT] then Right x2 else Left x1

showFTname :: Tp -> Maybe String 
showFTname tp = case tp of 
  (FunT x BoolT)        -> showFTname x
  (ClassT (ClsNm name)) -> Just $ lowercase name
  _                     -> Nothing

lowercase = map toLower

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
