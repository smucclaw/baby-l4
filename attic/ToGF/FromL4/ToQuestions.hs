{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.FromL4.ToQuestions where

import qualified Data.Set as S
import Data.Char (toLower)
import Questions
import L4.Syntax
import PGF
import ToGF.GenerateLexicon (createGF', printGF', AtomWithArity(..), GrName)

----------------------------------------------------------------------
-- Helper functions from GenerateLexicon specialised for Questions
grName :: GrName
grName = "Questions"

createGF :: Show t => FilePath -> Program t -> IO PGF
createGF fname prog = createGF' fname grName (lexiconOfProgram prog) allPreds
  where
    allPreds = S.toList $ S.fromList $ concat
      [ getAtoms $ toPred vardecl
      | vardecl <- varDeclsOfProgram prog
      , isPred vardecl ]

printGF :: Gf a => PGF -> a -> IO ()
printGF gr expr = printGF' gr (gf expr)

----------------------------------------------------------------------
-- Printing GF

getAtoms :: GPred -> [AtomWithArity]
getAtoms (GMkPred0 (LexAtom name)) = [AA name 0]
getAtoms (GMkPred1 (LexAtom name) (LexAtom arg)) = [AA name 1, AA arg 0]
getAtoms (GMkPred2 (LexAtom name) (LexAtom arg1) (LexAtom arg2)) = [AA name 2, AA arg1 0, AA arg2 0]

createQuestions :: Show t => FilePath -> Program t -> IO ()
createQuestions filename prog = do
  gr <- createGF filename prog
  let questions = toQuestions prog
  mapM_ (printGF gr) questions

----------------------------------------------------------------------
-- Type class for questions
class Questionable x where
    toQuestions :: x -> [GQuestion]

instance Show t => Questionable (VarDecl t) where
  toQuestions v = [GAreThereAny, GAreThereMore,  GProperties] <*>  [toPred v]

instance Show a => Questionable (Program a) where
  toQuestions = concatMap toQuestions . filter isPred.varDeclsOfProgram

toPred :: Show t => VarDecl t -> GPred
toPred d = case d of
  Pred1 name arg1      -> GMkPred1 (LexAtom (l name)) (LexAtom (l arg1))
  Pred2 name arg1 arg2 -> GMkPred2 (LexAtom (l name)) (LexAtom (l arg1)) (LexAtom (l arg2))
  VarDecl _ nm tp      -> error $  "The VarDecl '" ++ nm ++ " : " ++ show tp ++ "' is not a predicate :("
  where
    -- The atoms need to be lowercase to match the s(CASP) atoms
    l :: String -> String
    l = map toLower


isPred :: VarDecl t -> Bool
isPred = isPred' . tpOfVarDecl

isPred' :: Tp t -> Bool
isPred' (FunT _ _ (ClassT _ BooleanC)) = True
isPred' (FunT _ _ t2) = isPred' t2
isPred' _ = False

-- patterns

pattern Pred1 :: VarName -> String           -> VarDecl t
pattern Pred1 name arg1 <- VarDecl _ name (Arg1 arg1)  -- to create VarDecl String Tp

pattern Pred2 :: VarName -> String -> String -> VarDecl t
pattern Pred2 name arg1 arg2 <- VarDecl _ name (Arg2 arg1 arg2)

pattern Arg1 :: String -> Tp t
pattern Arg1 x <- FunT _ (ClassT _ (ClsNm x)) (ClassT _ BooleanC)

pattern Arg2 :: String -> String -> Tp t
pattern Arg2 x y <- FunT _ (ClassT _ (ClsNm x)) (Arg1 y)
