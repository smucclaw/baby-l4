{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.FromL4.ToAnswers where

import qualified Data.Set as S

import Syntax
import PGF
import ToGF.GenerateLexicon (createGF', AtomWithArity(..), GrName)

----------------------------------------------------------------------
-- Helper functions from GenerateLexicon specialised for Answers
grName :: GrName
grName = "Answer"

createGF :: FilePath ->Program t -> IO PGF
createGF fname prog = createGF' fname grName (lexiconOfProgram prog) allPreds
  where
    allPreds = S.toList $ S.fromList $ concat
      [ getAtoms vardecl
      | vardecl <- globalsOfProgram prog
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
getAtoms v = case v of
  Arity0 name typ -> [AA name 0, AA typ 0] -- Anything of form `rock : Sign`. Both the name (rock) and the type (Sign) are atoms.
  Pred1 name arg -> [AA name 1, AA arg 0]  -- Any unary predicate: `is_player : Person -> Bool`. Has to return Bool.
  Pred2 name arg1 arg2 -> [AA name 2, AA arg1 0, AA arg2 0] -- Any binary predicate: `win : Sign -> Sign -> Bool`. Has to return Bool.
  VarDecl _ name tp -> [AA name (getArity tp)] -- Any other function type: `salary : Person -> Int`. TODO: include also the types for this more generic case.

-- TODO: return AtomWithArity for the Tps in the main Tp.
-- For instance, foo : Business -> Person -> Int should return
-- [AA "foo" 2, AA "Business" 0, AA "Person" 0]
-- Make a test out of it!

getArity :: Tp t -> Int
getArity t = case t of
  FunT _ _ x -> 1 + getArity x
  _ -> 0

-- patterns
pattern Arity0 :: VarName -> String           -> VarDecl t
pattern Arity0 name typ <- VarDecl _ name (Arg0 typ)

pattern Pred1 :: VarName -> String           -> VarDecl t
pattern Pred1 name arg1 <- VarDecl _ name (Arg1 arg1)  -- to create VarDecl String Tp

pattern Pred2 :: VarName -> String -> String -> VarDecl t
pattern Pred2 name arg1 arg2 <- VarDecl _ name (Arg2 arg1 arg2)

pattern Arg0 :: String -> Tp t
pattern Arg0 x <- ClassT _ (ClsNm x)

pattern Arg1 :: String -> Tp t
pattern Arg1 x <- FunT _ (Arg0 x) (ClassT _ (ClsNm "Boolean"))

pattern Arg2 :: String -> String -> Tp t
pattern Arg2 x y <- FunT _ (Arg0 x) (Arg1 y)