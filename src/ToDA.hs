{-# LANGUAGE FlexibleInstances #-}

-- To DocAssemble! Which is to say, a bit of Python.

module ToDA where

import Syntax
import Paths_baby_l4
import System.Environment (withArgs)
import Control.Monad (forM_)
import Text.Printf (printf)
import Data.List (intercalate)

-- prettyprinting

-- in future, try to 
-- import some sort of YAML library so we aren't just doing all the prettyprinting ourselves
-- import some sort of Python library, ditto

-- a "Pythonic" data type can be dumped to python syntax.
-- for example if i have something like a class definition "MyClass" that i want to dump out to a Python program
-- the output would be "class MyClass:\n"

class Pythonic x where
  hiss :: x -> String
  hisslist :: [x] -> String
  hisslist xs = "[" ++ (intercalate ", " (hiss <$> xs)) ++ "]"

instance Pythonic Bool where
  hiss True  = "1" -- 1 is truthy. though we could also have True
  hiss False = "0" -- 0 is falsey. though we could also have False.

instance Pythonic a => Pythonic (Maybe a) where
  hiss Nothing = "0"
  hiss (Just x) = hiss x

instance Pythonic a => Pythonic [a] where
  hiss x = hisslist x


createDocAssemble :: (Show ct, Show et) => Program ct et -> IO ()
createDocAssemble p = putStrLn $ hiss p

instance Pythonic (Program ct et) where
  hiss (Program mapping classdecls vardecls rules assertions) =
    hiss classdecls

instance Pythonic (ClassDecl ct) where
  hiss (ClassDecl (ClsNm classname) (ClassDef t fielddecs)) =
    "class " ++ classname ++ ":\n" ++
    "  \"\"\"\n  this is a pythonic docstring\n  \"\"\"\n"
    ++ unlines (hiss <$> fielddecs)
  hisslist cds = intercalate "\n\n" (hiss <$> cds)

instance Pythonic FieldDecl where
  hiss (FieldDecl (FldNm fieldname) tp) =
    (take 4 $ repeat ' ') ++ "# " ++ fieldname ++ " is a " ++ tptype tp ++ "\n" ++
    (take 4 $ repeat ' ') ++ fieldname ++ " = " ++ hiss tp

tptype :: Tp -> String
tptype ( BoolT ) = "boolean"
tptype ( IntT ) = "int"
tptype ( ClassT (ClsNm "String") ) = "string"
tptype ( ClassT (ClsNm classname) ) = classname
tptype ( FunT tp1 tp2 ) = "function from " ++ show tp1 ++ " to " ++ show tp2
tptype ( TupleT tps ) = "tuple" 
tptype ( ErrT ) = "error"

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
