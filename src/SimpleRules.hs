{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module SimpleRules where

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
        -- usefulrules = ["accInad", "accAdIncAd", "accAdIncInad", "savingsAd", "savingsInad", "incomeAd", "incomeInadESteady", "incomeInadEUnsteady"]
        -- usefulsimple = [r | r <- validRules , nameOfSimpleRule r `elem` usefulrule

data SimpleRule t = SimpleRule {
                     nameOfSimpleRule :: String -- Maybe String
                   , varDeclsOfSimpleRule :: [VarDecl t]
                   , precondOfSimpleRule :: [Expr t]
                   , postcondOfSimpleRule :: Expr t }
                  --  , precondOfSimpleRule :: [ValidExpr t]
                  --  , postcondOfSimpleRule :: ValidExpr t}
  deriving (Eq, Ord, Show, Read, Functor)

-- data ValidExpr t = FApp (Expr t) | VExp (Expr t) deriving (Eq, Show)

-- exprToValidExpr :: Expr t -> Maybe (ValidExpr t)
-- exprToValidExpr fapp@AppE {} = Just $ FApp fapp
-- exprToValidExpr vexp@VarE {} = Just $ VExp vexp
-- exprToValidExpr _ = Nothing

-- Helper function that determines if a rule structure is a predicate
isRule :: Rule t -> Bool
isRule x
  | condValid (precondOfRule x) && condValid (postcondOfRule x) = True -- do i need to check if the rule has a name?
  | otherwise = False

-- Helper function for checking valid pre/post-condition
condValid :: Expr t -> Bool
condValid x = case x of
  BinOpE _ (BBool BBand) e1 e2-> condValid e1 || condValid e2
  AppE {} -> True
  _ -> False

-- Helper function to extract useful expressions within conjunctions
flattenConjs :: Expr t -> [Expr t]
flattenConjs (BinOpE _ (BBool BBand) e1 e2) = flattenConjs e1 <> flattenConjs e2
flattenConjs fApp@AppE {} = [fApp]
flattenConjs _ = []

-- validateConjs :: [Expr t] -> [ValidExpr t]
-- validateConjs = M.mapMaybe exprToValidExpr


-- rule <asodko>
-- for x : integer, y : integer
-- if makesMoney x && lotsSaved y && x                  -- [FApp (makesMoney...), FApp (lotsSaved...)]
-- then invest stocks

-- rule <vacuously true>
-- for x : boolean



-- TODO 1: some sort of preprocesing that removes illegal expressions (not applications of predicates to arguments)
-- DONE, to review
--    a note: isRule filters the rules that need processing, flattenConjs filters & processes the conjugates of said rules
ruleToSimpleRule :: Rule t -> Either String (SimpleRule t)
ruleToSimpleRule r
  | isRule r = Right $ SimpleRule 
                        (M.fromJust $ nameOfRule r) 
                        (varDeclsOfRule r) 
                        (flattenConjs (precondOfRule r)) 
                        (postcondOfRule r)
                        -- (exprToValidExpr $ postcondOfRule r)
  | otherwise = Left $ "Not a valid rule: " ++ M.fromJust (nameOfRule r) ++ "\n"


type PredName = String
type RuleName = String

data GrNode =
    PredOr PredName -- implicit Or
  | RuleAnd RuleName -- implicit And
  deriving (Eq, Ord, Show)

type GrEdge = (GrNode, GrNode)

instance Labellable GrNode where
  toLabelValue :: GrNode -> Label
  toLabelValue x = StrLabel (T.pack $ show x)

-- implicit Or
mkPredRule :: [SimpleRule t] -> PredName -> S.Set GrEdge
mkPredRule xs pname =
  let matchedRuleNames = [RuleAnd $ nameOfSimpleRule r | r <- xs, (funNameOfApp . postcondOfSimpleRule) r == Just pname]
  in S.fromList $ map (PredOr pname,) matchedRuleNames

-- implicit And
mkRulePred :: SimpleRule t -> S.Set GrEdge
mkRulePred r =
  let rname = nameOfSimpleRule r
      preConds = precondOfSimpleRule r
  --     preCondNames = S.unions $ funNameOfApp <$> preConds -- duplicates? 
  -- in S.map (RuleAnd rname,) $ S.map PredOr preCondNames
      preCondName = M.mapMaybe funNameOfApp preConds 
  in S.fromList $ map ((RuleAnd rname,) . PredOr) preCondName


-- TODO: flip direction of edges so that topologically sort begins with leaves
simpleRulesToGrNodes :: [SimpleRule t] -> (S.Set GrNode, S.Set GrEdge)
simpleRulesToGrNodes xs = let
  prednames = getAllRulePreds xs      -- prednames : Set PredName         -- we don't want duplicate predicate names
  rulenames = getAllRuleNames xs      -- rulenames : Set Rulename         -- no duplicate 
  prednodes = S.map PredOr prednames    -- prednodes : Set GrNode
  rulenodes = S.map RuleAnd rulenames   -- rulenodes : Set GrNode
  orruleedges = S.unions $ S.map (mkPredRule xs) prednames    -- orruleedges: Set GrEdge
  andprededges = S.unions $ map mkRulePred xs
  in
  (S.unions [prednodes,rulenodes],
   S.unions [andprededges, orruleedges])

simpleRuleToGrNodes :: SimpleRule t -> (S.Set GrNode, S.Set GrEdge)
simpleRuleToGrNodes r = let
  (preconds, postcond) = rulePreds' r
  rulename = nameOfSimpleRule r      -- rulenames : String
  precondnodes = S.map PredOr preconds -- prednodes : Set GrNode
  postcondnode = PredOr postcond
  rulenode = RuleAnd rulename   -- rulenodes : Set GrNode
  orruleedges = (postcondnode, rulenode) -- orruleedges: Set GrEdge
  andprededges = S.map (rulenode, ) precondnodes 
  in
  (S.union precondnodes $ S.fromList [postcondnode, rulenode],
   S.union andprededges $ S.singleton orruleedges)


getAllRulePreds :: [SimpleRule t] -> S.Set PredName
getAllRulePreds xs = S.unions $ map rulePreds xs

rulePreds :: SimpleRule t -> S.Set PredName
rulePreds (SimpleRule _ _ preConds postConds) = S.fromList $ M.mapMaybe funNameOfApp (postConds : preConds)

rulePreds' :: SimpleRule t -> (S.Set PredName, PredName)
rulePreds' (SimpleRule _ _ preConds postConds) = 
  ( S.fromList . M.mapMaybe funNameOfApp $ preConds
  , M.fromJust . funNameOfApp $ postConds )

-- TODO 3 : Dependent on TODO 1, change output to Expr t -> PredName
funNameOfApp :: Expr t -> Maybe PredName
funNameOfApp (AppE _ x _) = funNameOfApp x
funNameOfApp (VarE _ x) = Just $ nameOfQVarName $ nameOfVar x
funNameOfApp _ = Nothing

 
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
-- a ~ GrNode
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

-- TODO: case match on GrNode constructors for or/and nodes
-- alternatively, remove or/and nodes altogether
-- DONE: See GrNode

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
        (usnodes, usedges) = simpleRulesToGrNodes validRules
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


-- -- Goal: get a DOT formatted output, or maybe an SVG output from 

-- -- an input l4 file. The program should only act on predicates 
-- -- defined by "Rule" syntax.

-- -- Specifically, a predicate in l4 has 3 components
-- --  1) the name of the rule,
-- --  2) preconditions of rule
-- --  3) postconditions of rule
-- -- It might also have variable declarations

