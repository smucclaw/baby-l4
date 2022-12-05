{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.FromL4.ToAnswers where

import qualified Data.Set as S

import L4.Syntax as Syntax
import PGF
import ToGF.GenerateLexicon (createGF', AtomWithArity(..), GrName)
import ToGF.NormalizeSyntax (pattern IntT, pattern BoolT)

----------------------------------------------------------------------
-- Helper functions from GenerateLexicon specialised for Answers
grName :: GrName
grName = "Answer"

createGF :: FilePath -> Program t -> IO PGF
createGF fname prog = createGF' fname grName (lexiconOfProgram prog) allPreds
  where
    allPreds = S.toList $ S.fromList $ concat
      [ getAtoms vardecl
      | vardecl <- varDeclsOfProgram prog
      ]


createPGFforAnswers :: FilePath -> Program t -> IO ()
createPGFforAnswers filename prog = do
  _ <- createGF filename prog
  -- Feel free to remove this printout, it's just there so that people know that this has changed :-P
  putStrLn ("Created the PGF file .l4-generated/" ++ grName ++ "Top.pgf out of the L4 file " ++ filename ++ ".")
  putStrLn "This PGF file should be copied to the Docassemble server and be used for generating the answers."

----------------
-- get the atoms

getAtoms :: VarDecl t -> [AtomWithArity]
getAtoms (VarDecl _ name tp) =  --[AA name 2, AA (getArity tp1), AA (getArity tp2)]
    AA name (getArity tp) : [ AA nm 0 | nm <- getNames tp]

-- TODO: return AtomWithArity for the Tps in the main Tp.
-- For instance, foo : Business -> Person -> Int should return
-- [AA "foo" 2, AA "Business" 0, AA "Person" 0]

-- getVarName :: Tp -> String
-- getVarName vardecl _  =

getArity :: Tp t -> Int
getArity t = case t of
  FunT _ _ x -> 1 + getArity x
  _ -> 0

getNames :: Tp t -> [String]
getNames t = case t of
  IntT _ -> []
  BoolT _ -> []
  ClassT _ (ClsNm x) -> [x]
  FunT _ t1 t2 -> getNames t1 ++ getNames t2 -- handle tree recursiion in leaves
  TupleT _ tps -> concatMap getNames tps     -- handle tree recursiion in leaves
  _ -> []


-- patterns
pattern Arity0 :: VarName -> String           -> VarDecl t
pattern Arity0 name typ <- Syntax.VarDecl _ name (Arg0 typ)

pattern Pred1 :: VarName -> String           -> VarDecl t
pattern Pred1 name arg1 <- Syntax.VarDecl _ name (Arg1 arg1)  -- to create VarDecl String Tp

pattern Pred2 :: VarName -> String -> String -> VarDecl t
pattern Pred2 name arg1 arg2 <- Syntax.VarDecl _ name (Arg2 arg1 arg2)

pattern Arg0 :: String -> Tp t
pattern Arg0 x <- ClassT _ (ClsNm x)

pattern Arg1 :: String -> Tp t
pattern Arg1 x <- FunT _ (Arg0 x) (ClassT _ BooleanC)

pattern Arg2 :: String -> String -> Tp t
pattern Arg2 x y <- FunT _ (Arg0 x) (Arg1 y)

pattern FunPattern :: String -> Tp t -> Tp t ->  VarDecl t
-- VarDecl x y i
-- FunT (ClassT (ClsNm x)) (FunT (ClassT (ClsNm y)) (IntT <- i)
pattern FunPattern name tp1 tp2 <- VarDecl _ name (FunT _ tp1 tp2)

