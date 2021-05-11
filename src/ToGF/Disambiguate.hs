{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module ToGF.Disambiguate where

import ParsePredicates as PP
import PGF hiding (Tree)
import Data.Maybe (isJust, fromMaybe, listToMaybe, mapMaybe, catMaybes)
import Data.Monoid (Any (..))

-------------------------------------------------------
-- Interactive disambiguation questions

type Question = Expr

-- assuming for now that all Exprs can be turned into a unique question.
-- TODO: don't assume that, it's not true!
mkQuestions :: [Expr] -> [(Question, Expr)]
mkQuestions es = [
  (gf q, e)
  | e <- es -- es come from ParsePredicate, they are all parsed into start category. TODO newtypes or something for extra safety?
  , let prd = fg e :: GPredicate
  , let [q] = catMaybes $ questions <*> [prd] -- unsafe assumption
  ]

questions :: [Tree a -> Maybe GUtt]
questions = (listToMaybe .) <$> [toVerbalComplement, toCompoundNoun]

-- TODO: replace with the subject from L4 file.
-- UnauthorizedSharingFees : Business -> Bool, make Business the subject.
dummySubj :: GNP
dummySubj = GMassNP (GpartyX (GString "X"))

presIndVPS :: GVP -> GVPS
presIndVPS = GMkVPS (GTTAnt GTPres GASimul) GPPos

toVerbalComplement :: PP.Tree a -> [GUtt]
toVerbalComplement t = case t of
  Gerund vp -> [GVerbalComplement dummySubj (presIndVPS vp)]
  _ -> composOpMonoid toVerbalComplement t

toCompoundNoun :: PP.Tree a -> [GUtt]
toCompoundNoun t = case t of
  CompoundNoun vpslash np -> [GCompoundNoun dummySubj (presIndVPS (GComplSlash vpslash np))]
  _ -> composOpMonoid toCompoundNoun t

pattern Gerund :: GVP -> PP.Tree a
pattern Gerund vp <- GComplSlash (GSlashV2a _) (GMassNP (GGerundCN vp))

pattern CompoundNoun :: GVPSlash -> GNP -> PP.Tree a
pattern CompoundNoun vpslash np <- GComplSlash vpslash (getCompoundNoun -> [np]) 

getCompoundNoun :: PP.Tree a -> [GNP]
getCompoundNoun t = case t of
  GCompoundN n cn -> [GMassNP (GUseN (GCompoundNHyphen n cn))]
  _ -> composOpMonoid getCompoundNoun t  

getObj :: PP.Tree a -> GNP
getObj vp = case vp of
  GComplSlash _ np -> np
  GAdVVP adv vp -> getObj vp
  GAdvVP vp adv -> getObj vp
  GComplA2 _ np -> np
  GComplN2 _ np -> np
  _ -> error "Not a transitive phrase"
  -- GComplVA _ _
  -- GComplVQ _ _
  -- GComplVS _ _

-- TODO: fix grammar, bring back ambiguity, add smarter filters before reaching this point
removeAdjuncts :: PP.Tree a -> PP.Tree a
removeAdjuncts t = case t of 
  GAdVVP _ vp -> removeAdjuncts vp
  GAdvVP vp _ -> removeAdjuncts vp
  GAdVVPSlash _ vpslash -> removeAdjuncts vpslash
 -- GAdvVP
  GAdvVPSlash vpslash _ -> removeAdjuncts vpslash
  GAdAP _ ap -> removeAdjuncts ap
  GAdjCN _ cn -> removeAdjuncts cn
  GAdvCN cn _ -> removeAdjuncts cn
  GExtAdvNP np _ -> removeAdjuncts np
  _ -> composOp removeAdjuncts t

----------------------------------------------------

filterHeuristic :: Int -> [(a,Expr)] -> [(a,Expr)]
filterHeuristic ar ts_udts = [ (udt, extractLex t)
                        | (udt, t) <- ts_udts
                        , not $ ppBeforeAP t
                        , filterGerund t ]
                        --, filterArity t ]
  where
    ts = map snd ts_udts
    filterGerund
      | any hasGerund ts &&  -- "practicing as lawyer": progressive, pres. part. or gerund
        any hasProgr ts &&
        not (all hasGerund ts) = not . hasProgr
--      | any hasGerund ts && not (all hasGerund ts) = hasGerund
      | otherwise = const True

    -- filterArity
    --   | any arityMatches ts = arityMatches
    --   | otherwise = const True

    -- arityMatches t = getArity t == ar


    -- getArity :: Expr -> Arity
    -- getArity e = case (fg e :: GPredicate) of
    --                Gp2 {} -> 2
    --                GPredNP2 {} -> 2
    --                GPredAP2 {} -> 2
    --                GV2PartAdv {} -> 2
    --                Gp0 {} -> 0
    --                _ -> 1

extractLex :: Expr -> Expr
extractLex e =
  case (fg e :: GPredicate) of
--    GPredNP2 cn prep -> gf $ GmkAtom $ GMkCN2 cn prep
    GPredAP2 _ ap prep -> gf $ Gp2 $ GComplAP2 ap prep
    GV2PartAdv pol v2 adv -> gf $ Gp1 $ GComplAP (GAdvAP (GPastPartAP (GSlashV2a v2)) adv)
--    Gp0 cn -> gf $ Gp0 cn
    _ -> e

-- TODO only remove apposition, keep compound noun


hasGerund :: Expr -> Bool
hasGerund = getAny . hasGerund' . (fg :: Expr -> GPredicate)
  where
    hasGerund' :: Tree a -> Any
    hasGerund' (GGerundCN _) = Any True
    hasGerund' x = composOpMonoid hasGerund' x

hasProgr :: Expr -> Bool
hasProgr = getAny . hasProgr' . (fg :: Expr -> GPredicate)
  where
    hasProgr' :: Tree a -> Any
    hasProgr' (GProgrVP _) = Any True
    hasProgr' (GUseComp (GCompAP (GPresPartAP _))) = Any True
    hasProgr' x = composOpMonoid hasProgr' x

ppBeforeAP :: Expr -> Bool
ppBeforeAP = getAny . ppBeforeAP' . (fg :: Expr -> GPredicate)
  where
    ppBeforeAP' :: Tree a -> Any
    ppBeforeAP' (GAdjCN (GPastPartAP _) (GAdjCN _ _)) = Any True
    ppBeforeAP' (GAdjCN (GPastPartAgentAP _ _) (GAdjCN _ _)) = Any True
    ppBeforeAP' x = composOpMonoid ppBeforeAP' x
