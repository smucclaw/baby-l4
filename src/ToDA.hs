-- To DocAssemble! Which is to say, a bit of Python.

module ToDA where

import Syntax
import Paths_baby_l4
import System.Environment (withArgs)
import Control.Monad (forM_)
import Text.Printf (printf)

-- prettyprinting

-- in future, try to 
-- import some sort of YAML library so we aren't just doing all the prettyprinting ourselves
-- import some sort of Python library, ditto

createDocAssemble :: (Show ct, Show et) => Program ct et -> IO ()
createDocAssemble (Program mapping classdecls vardecls rules assertions) =
  putStrLn $ myshowcds classdecls

myshowcds :: (Show ct) => [ClassDecl ct] -> String
myshowcds cds = unlines $ "# CLASS DECLARATIONS!!!!!111" : (myshowcd <$> cds)

myshowcd :: (Show ct) => ClassDecl ct -> String
myshowcd (ClassDecl (ClsNm classname) (ClassDef t fielddecs)) =
  "class " ++ classname ++ ":\n"
  -- ++ " " ++ show t ++ "\n"
  ++ unlines (myshowfielddecl <$> fielddecs)

    -- FieldDecl (FldNm "name") (ClassT (ClsNm "String"))
    -- FieldDecl (FldNm "uen") (ClassT (ClsNm "String"))
    -- FieldDecl (FldNm "someNum") IntT

myshowfielddecl :: FieldDecl -> String
myshowfielddecl (FieldDecl (FldNm fieldname) tp) =
  (take 4 $ repeat ' ') ++ "# " ++ fieldname ++ " is a " ++ tptype tp ++ "\n" ++
  (take 4 $ repeat ' ') ++ fieldname ++ " = " ++ mytpassignment tp

tptype :: Tp -> String
tptype ( BoolT ) = "Boolean"
tptype ( IntT ) = "Int"
tptype ( ClassT (ClsNm "String") ) = "String"
tptype ( ClassT (ClsNm classname) ) = classname
tptype ( FunT tp1 tp2 ) = "function from " ++ show tp1 ++ " to " ++ show tp2
tptype ( TupleT tps ) = "tuple" 
tptype ( ErrT ) = "error"



mytpassignment :: Tp -> String
mytpassignment ( BoolT ) = "false"
mytpassignment ( IntT ) = "0"
mytpassignment ( ClassT (ClsNm "String") ) = "\"\""
mytpassignment ( ClassT classname ) = "moo"
mytpassignment ( FunT tp1 tp2 ) = "moo"
mytpassignment ( TupleT tps ) = "moo" 
mytpassignment ( ErrT ) = "moo"


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
