-- Visualization of the justiifications produced by expert system
{-# LANGUAGE DeriveAnyClass #-}

module JustificationVis where

import SimpleRules (LabelledGraph (LG), mkLabelledGraph, GraphOut (Propagation))

import Data.GraphViz
import Data.Graph.Inductive.Query.DFS
import Data.HashSet qualified as S

import L4.Syntax
import GHC.Generics (Generic)
import Data.Hashable

type ESArg = Val

-- | Logical Predicates
data ESPred = ESPred String [ESArg] [ESRule]
    deriving (Eq, Ord, Show)
-- | Priors are Predicates

-- | Rules
data ESRule = ESRule String [ESArg] [ESPred]
    deriving (Eq, Ord, Show)

data JGraphNode
    = JPred String [ESArg]
    | JRule String [ESArg]
    deriving (Eq, Generic, Hashable, Ord, Show)

instance Labellable JGraphNode where
    toLabelValue (JPred nm args) = toLabelValue ("P " ++ nm ++ " : " ++ show args)
    toLabelValue (JRule nm args) = toLabelValue ("R " ++ nm ++ " : " ++ show args)

type JGraphEdge = (JGraphNode, JGraphNode)

nodeOfESPred :: ESPred -> JGraphNode
nodeOfESPred (ESPred nm args rls) = JPred nm args
nodeOfESRule :: ESRule -> JGraphNode
nodeOfESRule (ESRule nm args preds) = JRule nm args

successorsOfESPred :: ESPred -> [JGraphNode]
successorsOfESPred (ESPred nm args rls) = map nodeOfESRule rls

successorsOfESRule :: ESRule -> [JGraphNode]
successorsOfESRule (ESRule nm args preds) = map nodeOfESPred preds

jGraphNodesOfESPred :: ESPred -> S.HashSet JGraphNode
jGraphNodesOfESPred p@(ESPred nm args rls) =
    S.union (S.singleton (nodeOfESPred p)) (S.unions $ map jGraphNodesOfESRule rls)

jGraphNodesOfESRule :: ESRule -> S.HashSet JGraphNode
jGraphNodesOfESRule r@(ESRule nm args preds) =
    S.union (S.singleton (nodeOfESRule r)) (S.unions $ map jGraphNodesOfESPred preds)

jGraphEdgesOfESPred :: ESPred -> S.HashSet JGraphEdge
jGraphEdgesOfESPred p@(ESPred nm args rls) =
    let ndsrc = nodeOfESPred p
    in S.union (S.fromList (map (ndsrc,) (successorsOfESPred p))) (S.unions $ map jGraphEdgesOfESRule rls)

jGraphEdgesOfESRule :: ESRule -> S.HashSet JGraphEdge
jGraphEdgesOfESRule r@(ESRule nm args preds) =
    let ndsrc = nodeOfESRule r
    in S.union (S.fromList (map (ndsrc,) (successorsOfESRule r))) (S.unions $ map jGraphEdgesOfESPred preds)



outcome :: ESPred
outcome = 
    ESPred "Investment" [StringV "Adam", StringV "Stocks"] [
        ESRule "AccAdIncAd" [StringV "Adam"] [
            ESPred "Savings_account" [StringV "Adam", StringV "Adequate"] [
                ESRule "SavingsAd" [StringV "Adam", IntV 22000, IntV 2] [
                    ESPred "Amount_saved" [StringV "Adam", IntV 22000] 
                        [ESRule "Amount_savedAdam" [StringV "Adam", IntV 22000] []]
                  , ESPred "Dependents" [StringV "Adam", IntV 2] [
                      ESRule "DependentsAdam" [StringV "Adam", IntV 2] []]]]
          , ESPred "Income" [StringV "Adam", StringV "Adequate"] [
                ESRule "IncomeAd" [StringV "Adam", StringV "Adequate"] [
                    ESPred "Earnings" [StringV "Adam", IntV 25000, IntV 2] [
                        ESRule "EarningsAdam" [StringV "Adam", IntV 25000] []]
                  , ESPred "Dependents" [StringV "Adam", IntV 2] [
                      ESRule "DependentsAdam" [StringV "Adam", IntV 2] []]]]]]

jvis :: IO FilePath
jvis = do
    let nds =jGraphNodesOfESPred outcome
        eds = jGraphEdgesOfESPred outcome
        propgr = mkLabelledGraph Propagation nds eds
        (LG gr _ _) = propgr
    runGraphviz (graphToDot quickParams gr) Pdf "justif.pdf"

-- >>> jvis
-- "justif.pdf"

