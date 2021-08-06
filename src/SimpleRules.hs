{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module SimpleRules (isRule, expSys ) where

-- import Parser (parseProgram)
-- import Annotation ( SRng )
import Syntax
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree ( Gr )
import qualified Data.Maybe as M
import qualified Data.List as L
import Data.GraphViz
import Data.Graph.Inductive.Query.DFS

import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
        -- usefulrules = ["accInad", "accAdIncAd", "accAdIncInad", "savingsAd", "savingsInad", "incomeAd", "incomeInadESteady", "incomeInadEUnsteady"]
        -- usefulsimple = [r | r <- validRules , nameOfSimpleRule r `elem` usefulrule

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
ruleToSimpleRule :: Rule t -> Either String (SimpleRule t)
ruleToSimpleRule r 
  | isRule r = Right $ SimpleRule (M.fromJust $ nameOfRule r) (varDeclsOfRule r) (flattenConjs (precondOfRule r)) (postcondOfRule r)
  | otherwise = Left $ "Not a valid rule: " ++ M.fromJust (nameOfRule r) ++ "\n"


type PredName = String
type RuleName = String 

data RuleNode = AndN RuleName -- string is the name of the rule 
              | OrN PredName
              | PredN PredName
              | RuleN RuleName
              deriving (Eq, Ord, Show)

type RuleEdge = (RuleNode, RuleNode)

data RuleNodeCompact =
    PredOr PredName -- implicit Or
  | RuleAnd RuleName -- implicit And
  deriving (Eq, Ord, Show)

type RuleEdgeCompact = (RuleNodeCompact, RuleNodeCompact)

-- TODO: case match on RuleNode constructors for or/and nodes
-- alternatively, remove or/and nodes altogether
instance Labellable RuleNode where
  toLabelValue :: RuleNode -> Label
  toLabelValue x = StrLabel (T.pack $ show x)

instance Labellable RuleNodeCompact where
  toLabelValue :: RuleNodeCompact -> Label
  toLabelValue x = StrLabel (T.pack $ show x)

mkPredOr :: PredName -> RuleEdge
mkPredOr x = (PredN x, OrN x)

mkRuleAnd :: RuleName -> RuleEdge
mkRuleAnd x = (RuleN x, AndN x)

-- implicit Or
mkPredRule :: [SimpleRule t] -> PredName -> S.Set RuleEdgeCompact
mkPredRule xs pname =
  let matchedRuleNames = [RuleAnd $ nameOfSimpleRule r | r <- xs, (funNameOfApp . postcondOfSimpleRule) r == S.singleton pname]
  in S.fromList $ map ((,) (PredOr pname)) matchedRuleNames

-- implicit And
mkRulePred :: SimpleRule t -> S.Set RuleEdgeCompact
mkRulePred r =
  let rname = nameOfSimpleRule r
      preConds = precondOfSimpleRule r
      preCondNames = S.unions $ funNameOfApp <$> preConds
  in S.map ((,) (RuleAnd rname)) $ S.map PredOr preCondNames

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
simpleRuleToRuleNode :: [SimpleRule t] -> (S.Set RuleNodeCompact, S.Set RuleEdgeCompact)
simpleRuleToRuleNode xs = let
  prednames = getAllRulePreds xs
  rulenames = getAllRuleNames xs
  prednodes = S.map PredOr prednames
  -- ornodes = S.map OrN prednames
  -- predoredges = S.map mkPredOr prednames
  rulenodes = S.map RuleAnd rulenames
  -- andnodes = S.map AndN rulenames
  -- ruleandedges = S.map mkRuleAnd rulenames
  orruleedges = S.unions $ S.map (mkPredRule xs) prednames
  andprededges = S.unions $ map mkRulePred xs
  in
  -- (S.unions [prednodes,ornodes,rulenodes,andnodes],
  --  S.unions [predoredges, andprededges, orruleedges, ruleandedges])
  (S.unions [prednodes,rulenodes],
   S.unions [andprededges, orruleedges])

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

data GraphOut = Dependency | Propagation deriving (Eq, Show)


mkPropagationGraph :: Eq a => S.Set a -> S.Set (a, a) -> LabelledGraph a
mkPropagationGraph = mkLabelledGraph Propagation

mkDependencyGraph :: Eq a => S.Set a -> S.Set (a, a) -> LabelledGraph a
mkDependencyGraph = mkLabelledGraph Dependency


-- | Makes either a propagation or dependency graph
-- a ~ RuleNode
mkLabelledGraph :: Eq a => GraphOut -> S.Set a -> S.Set (a, a) -> LabelledGraph a
mkLabelledGraph gOut nodeset edgeset =
  let nodelist = S.toList nodeset
      edgelist = S.toList edgeset
      nodeIndSym = zip [0..length nodelist-1] nodelist
      nodeSymInd = zip nodelist [0..length nodelist-1]
      gEdges = edgeSetSymToInd gOut nodeSymInd edgelist
  in LG (mkGraph nodeIndSym gEdges) nodeIndSym nodeSymInd

-- NInd should occur in assoc list
indToSym :: Eq a => [(a, b)] -> a -> b
indToSym xs a = M.fromJust $ L.lookup a xs

edgeSetSymToInd :: Eq a => GraphOut -> [(a, NInd)] -> [(a, a)] -> [(NInd, NInd, String)]
edgeSetSymToInd Propagation nodeSymInd = map (\(n1,n2) -> (indToSym nodeSymInd n2, indToSym nodeSymInd n1, ""))
edgeSetSymToInd Dependency nodeSymInd = map (\(n1,n2) -> (indToSym nodeSymInd n1, indToSym nodeSymInd n2, ""))

labelledSCC :: LabelledGraph a -> [[a]]
labelledSCC (LG gr indSym _) = map (map (indToSym indSym)) (scc gr)

-- TODO 4
-- topologically sort graph beginning from leaves
-- find min and max dependency of each node



 -- traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
 -- traverse :: (Rule t -> Either String (SimpleRule t)) -> [Rule t] -> Either String [SimpleRule t]
-- traverse :: (Rule t -> Maybe (SimpleRule t)) -> [Rule t] -> Maybe [SimpleRule t]

expSys :: Program (Tp ()) -> IO ()
expSys x = do
    let myrules = rulesOfProgram x
        errs = lefts $ map ruleToSimpleRule myrules
        validRules = rights $ map ruleToSimpleRule myrules
        (usnodes, usedges) = simpleRuleToRuleNode validRules 
        myPropLGGraph = mkLabelledGraph Propagation usnodes usedges
        myDepLGGraph = mkLabelledGraph Dependency usnodes usedges
        (LG mypropgraph _ _) = myPropLGGraph
        (LG mydepgraph _ _) = myDepLGGraph
    _ <- runGraphviz (graphToDot quickParams mypropgraph) Pdf "compactPropGraph.pdf"
    _ <- runGraphviz (graphToDot quickParams mydepgraph) Pdf "compactDepGraph.pdf"
    print $ topsort' mypropgraph
    print errs
    print validRules
        -- print $ labelledSCC mypropgraph
        -- return ()


-- TODO: Fix the tests
-- esUnitTests :: Program (Tp ()) -> IO ()
-- esUnitTests xs = defaultMain $ testGroup "Expert System Tests" [
--         testGroup "isRule" [
--             testCase "returns True for basicRule.l4" $ do
--                 '1' @?= '1'    
--         ]
--     ]
  -- describe "isRule" $ do
  --   it "returns True for \"accInad\"" $ do
  --     isRule (singleRule "accInad") `shouldBe` True
  --   it "returns True for \"savingsAd\"" $ do
  --     isRule (singleRule "savingsAd") `shouldBe` True
  -- where 
  --   myrules = rulesOfProgram x
  --   usefulrules = ["accInad", "accAdIncAd", "accAdIncInad", "savingsAd", "savingsInad", "incomeAd", "incomeInadESteady", "incomeInadEUnsteady"]
  --   singleRule x = head $ [r | r <- myrules, nameOfRule r == Just x]
    




-- -- Goal: get a DOT formatted output, or maybe an SVG output from 

-- -- an input l4 file. The program should only act on predicates 
-- -- defined by "Rule" syntax.

-- -- Specifically, a predicate in l4 has 3 components
-- --  1) the name of the rule,
-- --  2) preconditions of rule
-- --  3) postconditions of rule
-- -- It might also have variable declarations

-- Helper function that determines if a rule structure is a predicate
isRule :: Rule t -> Bool
isRule x
  | condValid (precondOfRule x) && condValid (postcondOfRule x) = True -- do i need to check if the rule has a name?
  | otherwise = False

-- Helper function for checking valid pre/post-condition
condValid :: Expr t -> Bool
condValid x = case x of 
  BinOpE _ (BBool BBand) e1 e2-> condValid e1 && condValid e2 
  AppE {} -> True
  _ -> False 

