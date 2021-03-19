{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- To DocAssemble! Which is to say, a bit of Python.

module ToDA where

import Syntax
import Paths_baby_l4
import System.Environment (withArgs)
import Control.Monad (forM_)
import Text.Printf (printf)
import Data.List (intercalate)
import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)

-- prettyprinting

-- in future, try to 
-- import some sort of YAML library so we aren't just doing all the prettyprinting ourselves
-- import some sort of Python library, ditto

-- a "Pythonic" data type can be dumped to python syntax.
-- for example if i have something like a class definition "MyClass" that i want to dump out to a Python program
-- the output would be "class MyClass:\n"

class Javascripty x where
  asjs :: x -> Doc ann
  asjsList :: [x] -> Doc ann
  asjsList xs = encloseSep lbracket rbracket comma (asjs <$> xs)

class AsYAML x where
  asyaml :: x -> Doc ann
  default asyaml :: (Pretty x) => x -> Doc ann
  asyaml x = pretty x
  asyamlList :: [x] -> Doc ann
  asyamlList xs = vsep (("-" <+>) <$> (asyaml <$> xs))

instance (AsYAML a) => AsYAML [a] where
  asyaml = asyamlList

instance AsYAML Int
instance AsYAML Bool
instance (Pretty a) => AsYAML (Maybe a)

instance AsYAML Char where
  asyamlList cs = pretty cs

class Pythonic x where
  hiss :: x -> Doc ann
  hisslist :: [x] -> Doc ann
  hisslist xs = encloseSep lbracket rbracket comma (hiss <$> xs)

instance Pythonic Bool where
  hiss True  = "True"  -- 1 is truthy. though we could also have True
  hiss False = "False" -- 0 is falsey. though we could also have False.

instance Pythonic a => Pythonic (Maybe a) where
  hiss Nothing = "0"
  hiss (Just x) = hiss x

instance Pythonic a => Pythonic [a] where
  hiss x = hisslist x


createDocAssemble :: (Show ct, Show et) => Program ct et -> IO ()
createDocAssemble p = putDoc $ (hiss p <> PP.line)




instance Pythonic (Program ct et) where
  hiss (Program mapping classdecls vardecls rules assertions) =
    vsep [ "from typing import NewType"
         , "## CLASS DECLARATIONS ##", hiss classdecls
         , "## VARIABLE DECLARATIONS ##", pyComment $ hiss vardecls
         ]

-- input:  decl AssociatedWith: LegalPractitioner -> Appointment -> Bool
-- output: def AssociatedWith(var1 : LegalPractitioner, var2 : Appointment) -> bool:

instance Pythonic (VarDecl) where
  hiss vd = viaShow vd

instance Pythonic (ClassDecl ct) where
  hiss (ClassDecl (ClsNm classname) (ClassDef t fielddecs)) =
    nest 4 . vsep $
    [ "class" <+> pretty classname <> ":"
    , dquotes dquote, "this is a pythonic docstring", dquotes dquote ]
    ++ (hiss <$> fielddecs)

  hisslist cds = concatWith (\x y -> x <> PP.line <> PP.line <> y) (hiss <$> cds)

-- pycomment :: Doc ann -> Doc ann
pyComment mydoc = vsep $ pretty . ("# " ++) <$> (lines $ show mydoc)

instance Pythonic FieldDecl where
  hiss (FieldDecl (FldNm fieldname) (FunT tp1 tp2)) =
    hang 4 (vsep [ hsep [ "def", pretty fieldname, encloseSep lparen rparen comma
                          [ pretty mvName <> ":" <+> tpt
                          | (mvName, tpt) <- zip metavariableNames (tptype <$> [tp1])
                          ]
                        , "->" <+> tptype tp2
                        , ":"]
                 , "# body of the function definition"
                 , "return None"
                 ] )
  hiss (FieldDecl (FldNm fieldname) tp) =
    vsep [ hsep ["#", pretty fieldname, "is a", tptype tp]
         , hsep [ pretty fieldname, typeannotation tp, "=" <+> hiss tp ] ]

metavariableNames = [ metavariableName ++ "_" ++ show i
                    | i <- [1..] ]
metavariableName = "foo"

typeannotation :: Tp -> Doc ann
typeannotation tp = ":" <+> tptype tp

tptype :: Tp -> Doc ann
tptype ( BoolT ) = "bool"
tptype ( IntT ) = "int"
tptype ( ClassT (ClsNm "String") ) = "str"
tptype ( ClassT (ClsNm classname) ) = viaShow classname

--     def bar(var1 : bool, var2: tuple) -> str:
--        return None

tptype ( FunT tp1 tp2 ) = tptype tp1 <+> "->" <+> tptype tp2
tptype ( TupleT tps ) = encloseSep lparen rparen comma (tptype <$> tps)
tptype ( ErrT ) = "error"

-- the "null" definition for a field declaration inside a class
instance Pythonic Tp where
  hiss ( BoolT ) = "false"
  hiss ( IntT ) = "0"
  hiss ( ClassT (ClsNm "String") ) = "\"\""
  hiss ( ClassT classname ) = "moo"
  hiss ( FunT tp1 tp2 ) = "moo"
  hiss ( TupleT tps ) = "(moo, moo)" 
  hiss ( ErrT ) = "moo"


-- data Program ct et = Program [Mapping] [ClassDecl ct] [VarDecl] [Rule et] [Assertion et]

{- mini.l4 contains:
   Program [Mapping (SRng {start = Pos {line = 4, col = 0},
                           end = Pos {line = 4, col = 21}
                          })
           "is_legal" "legal_1_A"
          ,Mapping (SRng {start = Pos {line = 5, col = 0}, end = Pos {line = 5, col = 24}}) "Business" "business_1_N"]
          [] -- ClassDecl ct -- empty
            [
               VarDecl "myNumber" IntT
              ,VarDecl "myBusiness" (ClassT (ClsNm "Business"))
              ,VarDecl "is_legal" (FunT (ClassT (ClsNm "Business")) BoolT)]
          [] -- no Rules
          [] -- no Assertions
-}

{- cr.l4 contains:

Program [Mapping (SRng {start = Pos {line = 4, col = 0}, end = Pos {line = 4, col = 21}}) "is_legal" "legal_1_A",Mapping (SRng {start = Pos {line = 5, col = 0}, end = Pos {line = 5, col = 24}}) "Business" "business_1_N"]
        [ClassDecl (ClsNm "Business") (ClassDef (Just (ClsNm "Object"))
        [FieldDecl (FldNm "name") (ClassT (ClsNm "String"))
        ,FieldDecl (FldNm "uen") (ClassT (ClsNm "String"))
        ,FieldDecl (FldNm "someNum") IntT])]
        [VarDecl "myNumber" IntT
        ,VarDecl "myBusiness" (ClassT (ClsNm "Business"))
        ,VarDecl "is_legal" (FunT (ClassT (ClsNm "Business")) BoolT)]
        [] -- no rules
        [] -- no assertions
-}

{- we want to transform the cr.l4 into valid Python: see test/forDocAssemble.py
-}

-- createYAML :: (Show ct, Show et) => Program ct et -> IO [String]
-- createYAML (Program lexicon _2 _3 _4 _5) = do
--   return $ [ "---" ]



-- nlg :: (Show ct, Show et) => Program ct et -> IO ()
-- nlg prog = do
--   yamlstrings <- createYAML prog
--   putStrLn <$> yamlstrings
--   sequence_ [ prop
--     | prop <- program2prop prog
--     ]

-- program2prop :: (Show ct, Show et) => Program ct et -> String
-- program2prop e = show e

-- -- later, convert "class" definitions to corresponding YAML/Python bitsies.
