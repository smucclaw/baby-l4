{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module SimpleRules ( expSysTest ) where

-- import Parser (parseProgram)
-- import Annotation ( SRng )
import Syntax
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Maybe as M
import qualified Data.List as L
import Data.GraphViz
import Data.Graph.Inductive.Query.DFS

import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T

data SimpleRule t = SimpleRule { 
                     nameOfSimpleRule :: String -- Maybe String
                   , varDeclsOfSimpleRule :: [VarDecl t]
                   , precondOfSimpleRule :: [Expr t]
                   , postcondOfSimpleRule :: Expr t}
  deriving (Eq, Ord, Show, Read, Functor)

flattenConjs :: Expr t -> [Expr t]
flattenConjs (BinOpE _ (BBool BBand) e1 e2) = flattenConjs e1 ++ flattenConjs e2
flattenConjs x = [x]

-- 1: some sort of preprocesing that removes illegal expressions (not applications of predicates to arguments)
ruleToSimpleRule :: Rule t -> SimpleRule t
ruleToSimpleRule r = 
    SimpleRule (M.fromJust $ nameOfRule r) (varDeclsOfRule r) (flattenConjs (precondOfRule r)) (postcondOfRule r)


type PredName = String
type RuleName = String 

data RuleNode = AndN RuleName -- string is the name of the rule 
              | OrN PredName
              | PredN PredName
              | RuleN RuleName
              deriving (Eq, Ord, Show)

type RuleEdge = (RuleNode, RuleNode)

-- TODO: case match on RuleNode constructors for or/and nodes
-- alternatively, remove or/and nodes altogether
instance Labellable RuleNode where
  toLabelValue :: RuleNode -> Label
  toLabelValue x = StrLabel (T.pack $ show x)


mkPredOr :: PredName -> RuleEdge
mkPredOr x = (PredN x, OrN x)

mkRuleAnd :: RuleName -> RuleEdge
mkRuleAnd x = (RuleN x, AndN x)

-- TODO 2: Dependent on TODO 1, replace comparison of set to comparison of strings
mkOrRule :: [SimpleRule t] -> PredName -> S.Set RuleEdge
mkOrRule xs pname =
  let matchedRuleNames = [RuleN $ nameOfSimpleRule r | r <- xs, (funNameOfApp . postcondOfSimpleRule) r == S.singleton pname]
  in 
  S.fromList $ map ((,) (OrN pname)) matchedRuleNames

mkAndPred :: SimpleRule t -> S.Set RuleEdge
mkAndPred x =
  let rname = nameOfSimpleRule x 
      preConds = precondOfSimpleRule x
      preCondNames = S.unions $ funNameOfApp <$> preConds
  in
  S.map ((,) (AndN rname)) $ S.map PredN preCondNames

-- TODO: flip direction of edges so that topologically sort begins with leaves
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

type NInd = Int
-- a is the node type
data LabelledGraph a = LG (Gr a String) [(NInd, a)] [(a, NInd)]
  deriving Show

-- a ~ RuleNode
mkLabelledGraph :: Eq a => S.Set a -> S.Set (a, a) -> LabelledGraph a
mkLabelledGraph nodeset edgeset =
  let nodelist = S.toList nodeset
      edgelist = S.toList edgeset
      nodeIndSym = zip [0..length nodelist-1] nodelist
      nodeSymInd = zip nodelist [0..length nodelist-1]
      ledges = edgeSetSymToInd nodeSymInd edgelist
  in LG (mkGraph nodeIndSym ledges) nodeIndSym nodeSymInd

-- NInd should occur in assoc list
indToSym :: Eq a => [(a, b)] -> a -> b
indToSym xs a = M.fromJust $ L.lookup a xs

edgeSetSymToInd :: Eq a => [(a, NInd)] -> [(a, a)] -> [(NInd, NInd, String)]
edgeSetSymToInd nodeSymInd = map (\(n1,n2) -> (indToSym nodeSymInd n2, indToSym nodeSymInd n1, ""))

labelledSCC :: LabelledGraph a -> [[a]]
labelledSCC (LG gr indSym _) = map (map (indToSym indSym)) (scc gr)

-- TODO 4
-- topologically sort graph beginning from leaves
-- find min and max dependency of each node

expSysTest :: Program (Tp ()) -> IO ()
expSysTest x = do
    -- print usnodes
    -- print usedges
    -- print $ mkLabelledGraph usnodes usedges

    _ <- runGraphviz (graphToDot quickParams mygraph) Pdf "graph4.pdf"
    print $ topsort' mygraph
    print $ labelledSCC myLGGraph
    -- return ()
  where 
    myrules = rulesOfProgram x 
    simplerules = ruleToSimpleRule <$> myrules
    usefulrules = ["accInad", "accAdIncAd", "accAdIncInad", "savingsAd", "savingsInad", "incomeAd", "incomeInadESteady", "incomeInadEUnsteady"]
    usefulsimple = [r | r <- simplerules, nameOfSimpleRule r `elem` usefulrules]
    (usnodes, usedges) = simpleRuleToRuleNode usefulsimple
    myLGGraph = mkLabelledGraph usnodes usedges
    (LG mygraph _ _) = myLGGraph
    dotfile = runGraphviz (graphToDot quickParams mygraph) Pdf "graph.pdf"


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

