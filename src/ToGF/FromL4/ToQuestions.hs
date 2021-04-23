{-# LANGUAGE PatternSynonyms #-}

module ToGF.FromL4.ToQuestions where

import Questions
import Syntax
import Control.Applicative
import PGF

hello prog =

    mapM_ (putStrLn . (showExpr [] . gf)) (toQuestions prog)

class Questionable x where
    toQuestions :: x -> [GQuestion]

instance Questionable (VarDecl t) where
  toQuestions v = [GAreThereAny, GAreThereMore,  GProperties] <*>  [toPred v]

instance Questionable (Program a) where
  toQuestions = concatMap toQuestions . filter isPred.globalsOfProgram

toPred :: VarDecl t -> GPred
toPred (Pred1 name arg1)      = GMkPred1 (LexName name) (LexAtom arg1)
toPred (Pred2 name arg1 arg2) = GMkPred2 (LexName name) (LexAtom arg1) (LexAtom arg2)

isPred :: VarDecl t -> Bool
isPred = isPred' . tpOfVarDecl

isPred' :: Tp -> Bool
isPred' (FunT t BoolT) = True
isPred' (FunT t t2) = isPred' t2
isPred' _ = False


--------------------
-- patterns

pattern Pred1 :: VarName -> String           -> VarDecl t
pattern Pred1 name arg1 <- VarDecl _ name (Arg1 arg1)  -- to create VarDecl String Tp

pattern Pred2 :: VarName -> String -> String -> VarDecl t
pattern Pred2 name arg1 arg2 <- VarDecl _ name (Arg2 arg1 arg2)

pattern Arg1 :: String -> Tp
pattern Arg1 x <- FunT (ClassT (ClsNm  x) ) _

pattern Arg2 :: String -> String -> Tp
pattern Arg2 x y <- FunT (ClassT (ClsNm  x) ) (Arg1 y)

{- (FunT
     IntT 
     (FunT IntT BoolT)) -}