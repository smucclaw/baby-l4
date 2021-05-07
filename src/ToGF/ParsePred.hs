{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module ToGF.ParsePred where


import ParsePredicates as PP
import qualified Predicates as P
import Data.Char (toLower, toUpper, isLower, isUpper)
import Data.List.Extra (trim, sort, transpose, allSame)
import Data.List.Split ( split, splitOn, startsWithOneOf, startsWith )
import Data.Monoid (Any (..))
import PGF hiding (Tree)
import Text.Printf (printf)
import Data.Maybe (isJust)
import UDConcepts ( UDSentence, prUDSentence )
import UDAnnotations (getEnv, initUDEnv, UDEnv (..))
import GF2UD
import Debug.Trace (trace)

-- trace :: p1 -> p2 -> p2
-- trace s a = a

----------------------------------------------------

fromLexicalNode :: PP.Tree a -> Maybe String
fromLexicalNode (LexA str) = Just str
fromLexicalNode (LexA2 str) = Just str
fromLexicalNode (LexAdV str) = Just str
fromLexicalNode (LexAdv str) = Just str
fromLexicalNode (LexCard str) = Just str
fromLexicalNode (LexDet str) = Just str
fromLexicalNode (LexN str) = Just str
fromLexicalNode (LexN2 str) = Just str
fromLexicalNode (LexPN str) = Just str
fromLexicalNode (LexPredet str) = Just str
fromLexicalNode (LexPrep str) = Just str
fromLexicalNode (LexPron str) = Just str
fromLexicalNode (LexQuant str) = Just str
fromLexicalNode (LexSubj str) = Just str
fromLexicalNode (LexV str) = Just str
fromLexicalNode (LexV2 str) = Just str
fromLexicalNode (LexV2A str) = Just str
fromLexicalNode (LexV2S str) = Just str
fromLexicalNode (LexV2V str) = Just str
fromLexicalNode (LexV3 str) = Just str
fromLexicalNode (LexVA str) = Just str
fromLexicalNode (LexVS str) = Just str
fromLexicalNode (LexVV str) = Just str
fromLexicalNode (LexN3 str) = Just str
fromLexicalNode (LexText str) = Just str
fromLexicalNode (LexV2Q str) = Just str
fromLexicalNode (GString str) = Just str
fromLexicalNode _ = Nothing

----------------------------------------------------

type Arity = Int

data Predicate = Pred {name :: String, description :: String, trees :: [MyParseOutput], arity :: Arity}

showMPO :: MyParseOutput -> String
showMPO (t,udt) = unlines [showExpr [] t, prUDSentence 1 udt]

instance Show Predicate where
  show (Pred nm _ [] ar) = printf "%s\narity %d, no parses" nm ar
  show (Pred nm _ ts ar) = printf "%s\narity %d\nparses\n%s" nm ar exprs
    where
      exprs = unlines $ map showMPO ts

type Name = String
type Description = [String]

parsePred :: UDEnv -> Arity -> Name -> String -> Predicate
parsePred udenv ar funname optdesc = trace 
   ("ts pre-filter: " ++ unlines (map showMPO ts) ++ "\n" ++
    "arity: " ++ show ar ++ ", ts post-filter: " ++ unlines (map showMPO (filterHeuristic ar ts))) 
    $ Pred nm (unwords desc) (filterHeuristic ar ts) ar
  where
    nm : _ = splitOn ":" funname
    nm' = unwords $ splitOn "_" nm -- take care of is_participant_in style
    desc = case optdesc of -- if no description provided, parse the name, e.g. DescribedInSection1
              [] -> map (trim . lower) $ split capsOrDigits nm'
              _  -> words optdesc
    ts = filter (not . hasMeta . fst) (parseGF udenv desc)
    capsOrDigits = startsWithOneOf $ ['A'..'Z']++['0'..'9']

    hasMeta :: PGF.Expr -> Bool
    hasMeta expr = go $ unapply expr
      where
        go (e, []) = isMeta e
        go (e, es) = or $ isMeta e : map (go . unapply) es

        isMeta = isJust . unMeta

    lower :: String -> String
    lower str = if all isUpper str
                    then str
                    else map toLower str

{- HLINT ignore expr2ud "Eta reduce" -}
expr2ud :: UDEnv -> Expr -> UDSentence 
expr2ud udenv expr = gf2ud udenv lang expr
  where 
    lang = head $ languages $ pgfGrammar udenv

type MyParseOutput = (Expr, UDSentence)

parseGF :: UDEnv -> Description -> [MyParseOutput]
parseGF udenv = go
  where
    gr = pgfGrammar udenv
    lang = head $ languages gr
    cat = startCat gr
    go :: Description -> [MyParseOutput]
    go ws = finalParse
      where
        (output, bstring) = parse_ gr lang cat Nothing (unwords ws)
        finalParse = case output of
          ParseOk ts -> [(t, expr2ud udenv t) | t <- ts]
          -- TODO figure out why this doesn't work (anymore or did it ever work properly?)
          -- ParseFailed n | n>1 && all isLower (ws !! (n-1)) ->
          --   go [ if ind==n   -- Try capitalising the word where parse failed
          --          then toUpper w : ord
          --          else w : ord
          --      | (ind, w:ord) <- zip [1..] ws ]
          -- ParseFailed 1 -> [] -- [gf GNoParse]
          -- ParseFailed n -> go (take (n-1) ws)  -- ++ [gf (GParseFailedAfterNTokens (GInt n))]
          -- ParseIncomplete | not $ null ws -> go (init ws)
          _ -> []

-- if we want [(Expr,BracketedString)]
-- filterHeuristic' :: [MyParseOutput] -> [MyParseOutput]
-- filterHeuristic' ps = [(t,b) | (t,b) <- ps, not $ ppBeforeAP t]

filterHeuristic :: Arity -> [MyParseOutput] -> [MyParseOutput]
filterHeuristic ar ts_udts = [ (extractLex t, udt)
                        | (t, udt) <- ts_udts
                        , not $ ppBeforeAP t
                        , filterGerund t
                        , filterArity t ]
  where
    ts = map fst ts_udts
    filterGerund
      | any hasGerund ts &&  -- "practicing as lawyer": progressive, pres. part. or gerund
        any hasProgr ts &&
        not (all hasGerund ts) = not . hasProgr
--      | any hasGerund ts && not (all hasGerund ts) = hasGerund
      | otherwise = const True

    filterArity
      | any arityMatches ts = arityMatches
      | otherwise = const True

    arityMatches t = getArity t == ar


    getArity :: Expr -> Arity
    getArity e = case (fg e :: GPredicate) of
                   Gp2 {} -> 2
                   GPredNP2 {} -> 2
                   GPredAP2 {} -> 2
                   GV2PartAdv {} -> 2
                   Gp0 {} -> 0
                   _ -> 1

extractLex :: Expr -> Expr
extractLex e = 
  case (fg e :: GPredicate) of
    GPredNP2 cn prep -> gf $ GmkAtom $ GMkCN2 cn prep
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
