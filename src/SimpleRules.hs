{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SimpleRules ( expSysTest ) where

import Parser (parseProgram)
import Syntax
import Annotation ( SRng )
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Maybe as M

data SimpleRule t = SimpleRule { 
                     nameOfSimpleRule :: String -- Maybe String
                   , varDeclsOfSimpleRule :: [VarDecl t]
                   , precondOfSimpleRule :: [Expr t]
                   , postcondOfSimpleRule :: Expr t}
  deriving (Eq, Ord, Show, Read, Functor)

flattenConjs :: Expr t -> [Expr t]
flattenConjs (BinOpE _ (BBool BBand) e1 e2) = flattenConjs e1 ++ flattenConjs e2
flattenConjs x = [x]

-- TODO 1: some sort of preprocesing that removes illegal expressions (not applications of predicates to arguments)
ruleToSimpleRule :: Rule t -> SimpleRule t
ruleToSimpleRule r = 
    SimpleRule (M.fromJust $ nameOfRule r) (varDeclsOfRule r) (flattenConjs (precondOfRule r)) (postcondOfRule r)

data AndOrTree i 
    = LeafT i 
    | AndT i [AndOrTree i]
    | OrT i [AndOrTree i]


type PredName = String
type RuleName = String 

data RuleNode = AndN RuleName -- string is the name of the rule 
              | OrN PredName
              | PredN PredName
              | RuleN RuleName
              deriving (Eq, Ord, Show)

data RuleEdge = RuleEdge RuleNode RuleNode deriving (Eq,Ord,Show)
 

mkPredOr :: PredName -> RuleEdge
mkPredOr x = RuleEdge (PredN x) (OrN x)

mkRuleAnd :: RuleName -> RuleEdge
mkRuleAnd x = RuleEdge (RuleN x) (AndN x)

-- TODO 2: Dependent on TODO 1, replace comparison of set to comparison of strings
mkOrRule :: [SimpleRule t] -> PredName -> S.Set RuleEdge
mkOrRule xs pname =
  let matchedRuleNames = [RuleN $ nameOfSimpleRule r | r <- xs, (funNameOfApp . postcondOfSimpleRule) r == S.singleton pname]
  in 
  S.fromList $ map (RuleEdge (OrN pname)) matchedRuleNames

mkAndPred :: SimpleRule t -> S.Set RuleEdge
mkAndPred x =
  let rname = nameOfSimpleRule x 
      preConds = precondOfSimpleRule x
      preCondNames = S.unions $ funNameOfApp <$> preConds
  in
  S.map (RuleEdge (AndN rname)) $ S.map PredN preCondNames


simpleRuleToRuleNode :: [SimpleRule t] -> (S.Set RuleNode, S.Set RuleEdge)
simpleRuleToRuleNode xs = let
  prednames = getAllRulePreds xs
  rulenames = getAllRuleNames xs
  prednodes = S.map PredN prednames
  ornodes = S.map OrN prednames
  predoredges = S.map mkPredOr prednames
  rulenodes = S.map RuleN rulenames
  andnodes = S.map AndN rulenames
  ruleandedges = S.map mkRuleAnd rulenames
  orruleedges = S.unions $ S.map (mkOrRule xs) prednames
  andprededges = S.unions $ map mkAndPred xs
  in
  (S.unions [prednodes,ornodes,rulenodes,andnodes],
   S.unions [predoredges, andprededges, orruleedges, ruleandedges])

getAllRulePreds :: [SimpleRule t] -> S.Set PredName
getAllRulePreds xs = S.unions $ map rulePreds xs

rulePreds :: SimpleRule t -> S.Set PredName
rulePreds (SimpleRule _ _ preConds postConds) = S.union (funNameOfApp postConds) (S.unions $ map funNameOfApp preConds)

-- TODO 3 : Dependent on TODO 1, change output to Expr t -> PredName
funNameOfApp :: Expr t -> S.Set PredName
funNameOfApp (AppE _ x _)= funNameOfApp x
funNameOfApp (VarE _ x) = S.singleton $ nameOfQVarName $ nameOfVar x
funNameOfApp _ = S.empty

getAllRuleNames :: [SimpleRule t] -> S.Set RuleName
getAllRuleNames xs = S.fromList $ map nameOfSimpleRule xs








expSysTest :: Program (Tp ()) -> IO ()
expSysTest x = do
    print usnodes
    print usedges
  where 
    myrules = rulesOfProgram x 
    simplerules = ruleToSimpleRule <$> myrules
    usefulrules = ["accInad", "accAdIncAd", "accAdIncInad"]
    usefulsimple = [r | r <- simplerules, nameOfSimpleRule r `elem` usefulrules]
    (usnodes, usedges) = simpleRuleToRuleNode usefulsimple

    
    
-- -- Goal: get a DOT formatted output, or maybe an SVG output from 

-- -- an input l4 file. The program should only act on predicates 
-- -- defined by "Rule" syntax.

-- -- Specifically, a predicate in l4 has 3 components
-- --  1) the name of the rule,
-- --  2) preconditions of rule
-- --  3) postconditions of rule
-- -- It might also have variable declarations

-- -- "Main"
-- outputDOT :: Program (Tp t) -> IO ()
-- outputDOT p = undefined 

-- -- check through rule structures and return list of 
-- -- predicates
-- retainRules :: [Rule t] -> [Maybe (Rule t)]
-- retainRules (x:xs) 
--   | isRule x = Just x : retainRules xs  -- i feel like there's a better way to do this
--   | otherwise = retainRules xs
-- retainRules [] = []

-- -- Helper function that determines if a rule structure is a predicate
-- isRule :: Rule t -> Bool
-- isRule x
--   | condValid precondOfRule x && condValid postcondOfRule x = True -- do i need to check if the rule has a name?
--   | otherwise = False

-- -- Helper function for checking valid pre/post-condition
-- condValid :: (Rule t -> Expr t) -> Rule t -> Bool
-- condValid f x = case f x of 
--   -- BinOpE _ (BBool BBand) e1 e2-> condValid e1 && condValid e2 
--   AppE {} -> True
--   _ -> False 

