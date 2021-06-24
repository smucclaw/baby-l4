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
data DSBlock t = DSBlock { blkName  :: String               -- block name
                         , blkType  :: String               --
                         , blkCard  :: Maybe Int            -- cardinality, everything is assumed to be a list
                         , blkEncodings :: String           -- associated sCASP encoding
                         , blkAttrs :: [Maybe (DSBlock t)]  --
                         , blkUI    :: [Maybe t]            -- ask, tell, any, another
                         , blkSource :: Maybe String        -- only appears if block is a child attribute, with an "Object type"
                         }


instance (Show t) => DSYaml (DSBlock t) where
  showDS DSBlock { blkName, blkType, blkCard, blkEncodings, blkAttrs, blkUI, blkSource} =
    hang 2 $ "-" <+> vsep [ "name:" <+> pretty blkName
                          , "type:" <+> pretty blkType
                          , putSource blkSource <> putAttrs blkAttrs
                          ]
      where putAttrs ba = case ba of
              [Nothing] -> PP.emptyDoc
              _  -> "attributes:" <> PP.line <> indent 2 (showDSlist $ catMaybes ba) -- attributes here (one-level only)
            putSource (Just x) = "source:" <+> pretty x <> PP.line
            putSource Nothing = PP.emptyDoc


--  nameOfClassDecl = ClsNm { stringOfClassName = "Service" }
--             , defOfClassDecl = ClassDef
--                 { supersOfClassDef =
--                     [ ClsNm { stringOfClassName = "Service" }
--                     , ClsNm { stringOfClassName = "Class" }
--                     , ClsNm { stringOfClassName = "Object" }
--                     ]
--                 , fieldsOfClassDef =
--                     [ FieldDecl
--                         { annotOfFieldDecl = LocTypeAnnot
--                             { locAnnot =
--                             , typeAnnot = OkT
--                             }
--                         , nameOfFieldDecl = FldNm { stringOfFieldName = "isLegalService" }
--                         , tpOfFieldDecl = ClassT
--                             ( ClsNm { stringOfClassName = "Boolean" } )
--                         }
--                     , FieldDecl
--                         { annotOfFieldDecl = LocTypeAnnot
--                             { locAnnot =
--                             , typeAnnot = OkT
--                             }
--                         , nameOfFieldDecl = FldNm { stringOfFieldName = "isLawRelatedService" }
--                         , tpOfFieldDecl = ClassT
--                             ( ClsNm { stringOfClassName = "Boolean" } )
--                         }
--                     , FieldDecl
--                         { annotOfFieldDecl = LocTypeAnnot
--                             { locAnnot =
--                             , typeAnnot = OkT
--                             }
--                         , nameOfFieldDecl = FldNm { stringOfFieldName = "isListedInFourthSchedule" }
--                         , tpOfFieldDecl = ClassT
--                             ( ClsNm { stringOfClassName = "Boolean" } )
--                         }
--                     ]
--                 }
--             }

-- nameOfClassDecl = ClsNm { stringOfClassName = "LegalPractitioner" }
--             , defOfClassDecl = ClassDef
--                 { supersOfClassDef =
--                     [ ClsNm { stringOfClassName = "LegalPractitioner" }
--                     , ClsNm { stringOfClassName = "Person" } -- TODO: source for class decls that transpile to objects
--                     , ClsNm { stringOfClassName = "Class" }
--                     , ClsNm { stringOfClassName = "Object" }
--                     ]
--                 , fieldsOfClassDef =
--                     [ FieldDecl
--                         { annotOfFieldDecl = LocTypeAnnot
--                             { locAnnot =
--                             , typeAnnot = OkT
--                             }
--                         , nameOfFieldDecl = FldNm { stringOfFieldName = "Locum_Solicitor" }
--                         , tpOfFieldDecl = ClassT
--                             ( ClsNm { stringOfClassName = "Boolean" } )
--                         }
--                     , FieldDecl
--                         { annotOfFieldDecl = LocTypeAnnot
--                             { locAnnot =
--                             , typeAnnot = OkT
--                             }
--                         , nameOfFieldDecl = FldNm { stringOfFieldName = "Primary_Occupation" }
--                         , tpOfFieldDecl = ClassT
--                             ( ClsNm { stringOfClassName = "Boolean" } )
--                         }
--                     ]
--                 }
--             }


classDeclToBlock :: ClassDecl ct -> DSBlock ct
classDeclToBlock ClassDecl { nameOfClassDecl, defOfClassDecl } =
  let clnm = getClassname nameOfClassDecl
      super = getSuper $ supersOfClassDef defOfClassDecl
  in
  DSBlock { blkName = lowercase clnm
          , blkType = superToBlocktype super
          , blkEncodings = lowercase clnm ++ "(X)" -- TODO: Arity of parent encodings
          , blkAttrs = mapAttrs $ fieldsOfClassDef defOfClassDecl
          , blkUI = undefined
          , blkSource = superToBlocksource super
          }
  where getClassname (ClsNm name)= name
        mapAttrs x = case x of
          [] -> [Nothing]
          _  -> map (Just . fieldDeclToBlock) x
        getSuper (x1:x2:xs) = getClassname x2
        superToBlocktype x = case x of
          "Class" -> "String"
          _       -> "Object"
        superToBlocksource x = case x of
          "Class" -> Nothing
          x       -> Just $ lowercase x

fieldDeclToBlock :: FieldDecl ct -> DSBlock ct
fieldDeclToBlock (FieldDecl _ (FldNm fldnm) fieldtype) =
  let blockArity = getArity fieldtype
      blockType = getBlocktype fieldtype
      outname = lowercase fldnm
  in
  DSBlock { blkName = outname
          , blkType = blockType
          , blkEncodings = showEncoding outname blockArity
          , blkAttrs = [Nothing]
          , blkUI = undefined
          , blkSource = showFTname fieldtype  -- for supported types, there is no source block
          }


eitherTp :: String -> a -> Tp -> Either String a
eitherTp x1 x2 tp = if elem tp [BoolT, IntT] then Right x2 else Left x1

getBlocktype :: Tp -> String
getBlocktype tp = case tp of
  (FunT x y)  -> "Object"
  (ClassT (ClsNm "Boolean")) -> "Boolean"

showFTname :: Tp -> Maybe String
showFTname tp = case tp of
  (FunT x y)        -> showFTname x
  (ClassT (ClsNm "Boolean")) -> Nothing
  (ClassT (ClsNm name)) -> Just $ lowercase name
  _                     -> Nothing

getArity :: Tp -> Int
getArity (FunT x y) = 1 + getArity y
getArity (ClassT x) = 1

showEncoding :: String -> Int -> String
showEncoding x 1 = x <> "(X)"
showEncoding x 2 = x <> "(X,Y)"

lowercase :: String -> String
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
