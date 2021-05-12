{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module ToGF.Disambiguate where

import ParsePredicates as PP
import PGF hiding (Tree)
import Data.Monoid (Any (..))
import Data.List.Split ( splitOn )
import qualified Data.Map as M
import Control.Arrow (second)

-------------------------------------------------------
-- Interactive disambiguation questions

type Question = PGF.Expr

-- Questions

type QuestionMap = M.Map String [Expr]

-- TODO: use ReaderT or something
mkQuestionMap :: PGF -> [(Question,Expr)] -> QuestionMap
mkQuestionMap pgf qs_es = mkMap [(linearizeQuestion pgf q, e) | (q,e) <- qs_es]

showQuestionMap :: QuestionMap -> String
showQuestionMap = unlines . map showQuestionExpr . M.toList

showQuestionExpr :: (String,[Expr]) -> String
showQuestionExpr ("higher education", es) = showQuestionExpr ("NO QUESTIONS YET", es) -- TODO: have it print out something else than "higher education" :-D
showQuestionExpr (s,es) = unlines $ s : map (showExpr []) es

----------------------------------------------------


linearizeQuestion :: PGF -> Question -> String
linearizeQuestion gr = removeBind . linearize gr lang
  where
    lang = head $ languages gr
    removeBind = concat . splitOn " &+ "

type Subject = String

fg' :: PGF.Expr -> GPredicate
fg' = fg

mkQuestions :: [Expr] -> [(Question, Expr)]
mkQuestions es = [ (head $ concat qs, e) | (qs,e) <- mkQuestions' es]
-- assuming for now that all Exprs can be turned into a unique question.
-- TODO: don't assume that, it's not true!
mkQuestions' :: [Expr] -> [([[Question]], PGF.Expr)]
mkQuestions' es = [
  -- ((map.fmap) gf qs, gf $ removeAdjuncts $ fg' e)
  ((map.fmap) gf qs, e)
  | e <- es -- es come from ParsePredicate, they are all parsed into start category. TODO newtypes or something for extra safety?
  , let prd = removeAdjuncts $ fg e :: GPredicate
  , let qs = case  [f $ hyphenateCompoundNouns prd | f <- questions] of -- unsafe assumption
              -- [] -> [Just $ Gp0 Ghigher_education_CN]
              [[],[],[],[]] -> [pure $ Gp0 Ghigher_education_CN]
              xs -> xs
  ]

questions :: [Tree a -> [GPredicate]]
questions = [
    toVerbalComplement
  , askCompoundNoun
  , askVPSlash
  , askAdjCN
  ]

-- TODO: replace with the subject from L4 file.
-- UnauthorizedSharingFees : Business -> Bool, make Business the subject.
dummySubj, dummyObj :: GNP
dummySubj = GMassNP (GpartyX (GString "X"))
dummyObj = GMassNP (GpartyX (GString "Y"))

disambQ :: GVP -> GPredicate
disambQ = GPredSentence dummySubj . presIndVPS

presIndVPS :: GVP -> GVPS
presIndVPS = GMkVPS (GTTAnt GTPres GASimul) GPPos
presIndVPS2 :: GVPSlash -> GVPS2
presIndVPS2 = GMkVPS2 (GTTAnt GTPres GASimul) GPPos

toVerbalComplement :: PP.Tree a -> [GPredicate]
toVerbalComplement t = case t of
  Gerund vp -> [disambQ vp]
  _ -> composOpMonoid toVerbalComplement t

pattern Gerund :: GVP -> PP.Tree a
pattern Gerund vp <- GComplSlash (GSlashV2a _) (GMassNP (GGerundCN vp))


askCompoundNoun :: PP.Tree a -> [GPredicate]
askCompoundNoun t = case t of
  CompoundNoun vpslash np -> [disambQ (GComplSlash vpslash np)]
  GPrepNP prep (getCompoundNoun -> [np]) -> [disambQ (GUseComp (GCompAdv (GPrepNP prep np)))]
  _ -> composOpMonoid askCompoundNoun t

askAdjCN :: PP.Tree a -> [GPredicate]
askAdjCN t = case t of
  GAdjCN _ap _cn -> [disambQ (GUseComp (GCompNP (mkNP t)))]
  _ -> composOpMonoid askAdjCN t

pattern CompoundNoun :: GVPSlash -> GNP -> PP.Tree a
pattern CompoundNoun vpslash np <- GComplSlash vpslash (getCompoundNoun -> [np])

-- Different VPSlashes
askVPSlash :: PP.Tree a -> [GPredicate]
askVPSlash t = case t of
  VPSlashTrans vpslash -> [disambQ (GComplSlash vpslash dummyObj)]
  VPSlashIntrans vpslash -> [GPredSentence2 dummySubj (presIndVPS2 vpslash)]
  GPastPartAP _ -> [disambQ (GUseComp (GCompAP t))]
  _ -> composOpMonoid askVPSlash t

-- TODO: custom GF function to verbalise transitive verb used intransitively. Also use arity heuristic smarter.

pattern VPSlashTrans :: GVPSlash -> Tree a
pattern VPSlashTrans vpslash <- GComplVPSlash2 (GMkVPS2 _ _ vpslash)
pattern VPSlashIntrans :: GVPSlash -> Tree a
pattern VPSlashIntrans vpslash <- GComplVPSlash1 (GMkVPS2 _ _ vpslash)

getCompoundNoun :: PP.Tree a -> [GNP]
getCompoundNoun t = case removeAdjuncts t of
  e@(GMassNP (GUseN (GCompoundNHyphen _ _))) -> [e]
  e@(GDetCN _ (GUseN (GCompoundNHyphen _ _))) -> [e]
  _ -> []
  -- _ -> composOpMonoid getCompoundNoun t

hyphenateCompoundNouns :: PP.Tree a -> PP.Tree a
hyphenateCompoundNouns t = case t of
  GCompoundN n cn -> GCompoundNHyphen (hyphenateCompoundNouns n) (hyphenateCompoundNouns cn)
  _ -> composOp hyphenateCompoundNouns t


mkNP :: GCN -> GNP
mkNP = GDetCN (GDetQuant (LexQuant "IndefArt") GNumSg)

getObj :: PP.Tree a -> GNP
getObj t = case t of
  GComplSlash _ np -> np
  GAdVVP _adv vp -> getObj vp
  GAdvVP vp _adv -> getObj vp
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
  --GAdjCN _ cn -> removeAdjuncts cn
  GAdvCN cn _ -> removeAdjuncts cn
  GAdvNP np _ -> removeAdjuncts np
  GExtAdvNP np _ -> removeAdjuncts np
  _ -> composOp removeAdjuncts t

----------------------------------------------------

filterHeuristic :: Int -> [(a,Expr)] -> [(a,Expr)]
filterHeuristic _ar ts_udts = [ (udt, extractLex t)
                        | (udt, t) <- ts_udts
                        , not $ ppBeforeAP t
                        , filterGerund t
                        , filterAdvNP t
                        , filterAdvGerund t
                        , filterAdvAPPP t ]
                        --, filterArity t ]
  where
    ts = map snd ts_udts

    mayFilter f
      | any f ts && not (all f ts) = not . f
      | otherwise = const True

    -- "practicing as lawyer": progressive, pres. part. or gerund
    filterGerund
      | any hasGerund ts &&  -- "practicing as lawyer": progressive, pres. part. or gerund
        any hasProgr ts &&
        not (all hasGerund ts) = not . hasProgr
      | otherwise = const True

    filterAdvNP = mayFilter hasAdvNP
    filterAdvGerund = mayFilter advAttachesToGerundCN
    filterAdvAPPP = mayFilter hasPastPartAdvAPBy

    -- filterArity = mayFilter arityMatches

    -- arityMatches t = getArity t == ar


    -- getArity :: PGF.Expr -> Arity
    -- getArity e = case (fg e :: GPredicate) of
    --                Gp2 {} -> 2
    --                GPredNP2 {} -> 2
    --                GPredAP2 {} -> 2
    --                GV2PartAdv {} -> 2
    --                Gp0 {} -> 0
    --                _ -> 1

extractLex :: PGF.Expr -> PGF.Expr
extractLex e =
  case (fg e :: GPredicate) of
--    GPredNP2 cn prep -> gf $ GmkAtom $ GMkCN2 cn prep
    GPredAP2 _ ap prep -> gf $ Gp2 $ GComplAP2 ap prep
    GV2PartAdv _pol v2 adv -> gf $ Gp1 $ GComplAP (GAdvAP (GPastPartAP (GSlashV2a v2)) adv)
--    Gp0 cn -> gf $ Gp0 cn
    _ -> e

-- TODO only remove apposition, keep compound noun


-- Match specific GF constructors
-- Lot of boilerplate here, feel free to suggest improvements :-P
hasGerund :: PGF.Expr -> Bool
hasGerund = getAny . hasGerund' . fg'
  where
    hasGerund' :: Tree a -> Any
    hasGerund' (GGerundCN _) = Any True
    hasGerund' x = composOpMonoid hasGerund' x

hasProgr :: PGF.Expr -> Bool
hasProgr = getAny . hasProgr' . fg'
  where
    hasProgr' :: Tree a -> Any
    hasProgr' (GProgrVP _) = Any True
    hasProgr' (GUseComp (GCompAP (GPresPartAP _))) = Any True
    hasProgr' x = composOpMonoid hasProgr' x

ppBeforeAP :: PGF.Expr -> Bool
ppBeforeAP = getAny . ppBeforeAP' . fg'
  where
    ppBeforeAP' :: Tree a -> Any
    ppBeforeAP' (GAdjCN (GPastPartAP _) (GAdjCN _ _)) = Any True
    ppBeforeAP' (GAdjCN (GPastPartAgentAP _ _) (GAdjCN _ _)) = Any True
    ppBeforeAP' x = composOpMonoid ppBeforeAP' x

advAttachesToGerundCN :: PGF.Expr -> Bool
advAttachesToGerundCN = getAny . advGerund' . fg'
  where
    advGerund' :: Tree a -> Any
    advGerund' (GAdvCN (GGerundCN _) _) = Any True
    advGerund' x = composOpMonoid advGerund' x

hasAdvNP :: PGF.Expr -> Bool
hasAdvNP = getAny . hasAdvNP' . fg'
  where
    hasAdvNP' :: Tree a -> Any
    hasAdvNP' (GAdvNP _ _) = Any True
    hasAdvNP' x = composOpMonoid hasAdvNP' x

hasPastPartAdvAPBy :: PGF.Expr -> Bool
hasPastPartAdvAPBy =  getAny . hasPastPartAdvAPBy' . fg'
  where
    hasPastPartAdvAPBy' :: Tree a -> Any
    hasPastPartAdvAPBy' (GAdvAP (GPastPartAP _) (GPrepNP (LexPrep "by_Prep") _)) = Any True
    hasPastPartAdvAPBy' x = composOpMonoid hasPastPartAdvAPBy' x

hasAdvCN :: PGF.Expr -> Bool
hasAdvCN = getAny . hasAdvCN' . fg'
  where
    hasAdvCN' :: Tree a -> Any
    hasAdvCN' (GAdvCN _ _) = Any True
    hasAdvCN' x = composOpMonoid hasAdvCN' x


hasNAV2 :: PGF.Expr -> Bool
hasNAV2 = getAny . hasNAV2' . fg'
  where
    hasNAV2' :: Tree a -> Any
    hasNAV2' (GComplN2 _ _) = Any True
    hasNAV2' (GComplA2 _ _) = Any True
    hasNAV2' x = composOpMonoid hasNAV2' x

--hasAdjunct :: PGF.Expr -> Bool


-- | Make a Map where entries with colliding keys are collected into a list
mkMap :: (Ord k) => [(k, v)] -> M.Map k [v]
mkMap = M.fromListWith (<>) . map (second pure)