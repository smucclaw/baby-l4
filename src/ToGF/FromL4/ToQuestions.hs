{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.FromL4.ToQuestions where

import qualified Data.Set as S

import Questions
import Syntax
import PGF
import ToGF.GenerateLexicon (createGF', printGF', AtomWithArity(..), GrName)

----------------------------------------------------------------------
-- Helper functions from GenerateLexicon specialised for Questions
grName :: GrName
grName = "Questions"

createGF :: Program t -> IO PGF
createGF prog = createGF' grName allPreds
  where
    allPreds = S.toList $ S.fromList $ concat
      [ getAtoms $ toPred vardecl
      | vardecl <- globalsOfProgram prog
      , isPred vardecl ]

printGF :: Gf a => PGF -> a -> IO ()
printGF gr expr = printGF' gr (gf expr)

----------------------------------------------------------------------
-- Printing GF

getAtoms :: GPred -> [AtomWithArity]
getAtoms (GMkPred0 (LexAtom name)) = [AA name 0] 
getAtoms (GMkPred1 (LexAtom name) (LexAtom arg)) = [AA name 1, AA arg 0]
getAtoms (GMkPred2 (LexAtom name) (LexAtom arg1) (LexAtom arg2)) = [AA name 2, AA arg1 0, AA arg2 0]

createQuestions :: Program t -> IO ()
createQuestions prog = do
  gr <- createGF prog -- TODO: involve lexicon
  let questions = toQuestions prog
  mapM_ (printGF gr) questions

----------------------------------------------------------------------
-- Type class for questions
class Questionable x where
    toQuestions :: x -> [GQuestion]

instance Questionable (VarDecl t) where
  toQuestions v = [GAreThereAny, GAreThereMore,  GProperties] <*>  [toPred v]

instance Questionable (Program a) where
  toQuestions = concatMap toQuestions . filter isPred.globalsOfProgram

toPred :: VarDecl t -> GPred
toPred (Pred1 name arg1)      = GMkPred1 (LexAtom name) (LexAtom arg1)
toPred (Pred2 name arg1 arg2) = GMkPred2 (LexAtom name) (LexAtom arg1) (LexAtom arg2)

isPred :: VarDecl t -> Bool
isPred = isPred' . tpOfVarDecl

isPred' :: Tp -> Bool
isPred' (FunT t BoolT) = True
isPred' (FunT t t2) = isPred' t2
isPred' _ = False

-- patterns

pattern Pred1 :: VarName -> String           -> VarDecl t
pattern Pred1 name arg1 <- VarDecl _ name (Arg1 arg1)  -- to create VarDecl String Tp

pattern Pred2 :: VarName -> String -> String -> VarDecl t
pattern Pred2 name arg1 arg2 <- VarDecl _ name (Arg2 arg1 arg2)

pattern Arg1 :: String -> Tp
pattern Arg1 x <- FunT (ClassT (ClsNm x)) BoolT

pattern Arg2 :: String -> String -> Tp
pattern Arg2 x y <- FunT (ClassT (ClsNm x)) (Arg1 y)
