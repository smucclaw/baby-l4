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


-- Goal: get a DOT formatted output, or maybe an SVG output from 
-- an input l4 file. The program should only act on predicates 
-- defined by "Rule" syntax.

-- Specifically, a predicate in l4 has 3 components
--  1) the name of the rule,
--  2) preconditions of rule
--  3) postconditions of rule
-- It might also have variable declarations

-- "Main"
outputDOT :: Program (Tp t) -> IO ()
outputDOT p = undefined 

-- check through rule structures and return list of 
-- predicates
retainRules :: [Rule t] -> [Maybe (Rule t)]
retainRules (x:xs) 
  | isRule x = Just x : retainRules xs  -- i feel like there's a better way to do this
  | otherwise = retainRules xs
retainRules [] = []

-- Helper function that determines if a rule structure is a predicate
isRule :: Rule t -> Bool
isRule x
  | condValid precondOfRule x && condValid postcondOfRule x = True -- do i need to check if the rule has a name?
  | otherwise = False

-- Helper function for checking valid pre/post-condition
condValid :: (Rule t -> Expr t) -> Rule t -> Bool
condValid f x = case f x of 
  BinOpE {} -> True 
  AppE {} -> True
  _ -> False 


noSegFaultsPlease :: Bool 
noSegFaultsPlease = isRule testRule
