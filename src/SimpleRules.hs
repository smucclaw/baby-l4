{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SimpleRules where

import Parser (parseProgram)
import Syntax
import Annotation ( SRng )

data SimpleRule t = SimpleRule { 
                     nameOfSimpleRule :: ARName
                   , varDeclsOfSimpleRule :: [VarDecl t]
                   , precondOfSimpleRule :: [Expr t]
                   , postcondOfSimpleRule :: Expr t}
  deriving (Eq, Ord, Show, Read, Functor)

flattenConjs :: Expr t -> [Expr t]
flattenConjs (BinOpE _ (BBool BBand) e1 e2) = flattenConjs e1 ++ flattenConjs e2
flattenConjs x = [x]

ruleToSimpleRule :: Rule t -> SimpleRule t
ruleToSimpleRule r = 
    SimpleRule (nameOfRule r) (varDeclsOfRule r) (flattenConjs (precondOfRule r)) (postcondOfRule r)

data AndOrTree i 
    = LeafT i 
    | AndT i [AndOrTree i]
    | OrT i [AndOrTree i]
