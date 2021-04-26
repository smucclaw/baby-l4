{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module ToGF.ParsePred where


import ParsePredicates
import Paths_baby_l4 ( getDataFileName )
import Data.Char (toLower, toUpper, isLower)
import Data.List.Extra (trim, sort, transpose, allSame)
import Data.List.Split ( split, splitOn, startsWithOneOf )
import Data.Monoid (Any (..))
import PGF hiding (Tree)
import Text.Printf (printf)

----------------------------------------------------

fromLexicalNode :: Tree a -> Maybe String
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


--- >>> getPGF

getPGF :: IO PGF
getPGF = do
  pgf <- getDataFileName "ParsePredicates.pgf"
  readPGF pgf


----------------------------------------------------

data Predicate = Pred {name :: String, trees :: MyParseOutput, arity :: Int}

instance Show Predicate where
  show (Pred nm [] ar) = printf "%s\narity %d, no parses" nm ar
  show (Pred nm ts ar) = printf "%s\narity %d\nparses\n%s" nm ar exprs
    where
      exprs = unlines $ map (showExpr []) ts

type Question = [String]

step1 :: Predicate -> [Question]
step1 pr | length (trees pr) <= 1 = []
         | otherwise = mkQuestion (extractContentWords pr)


-- TODO: use the information from position and function to ask proper questions:
-- "is sole a verb or a noun"
-- and that makes other options impossible
-- Ask first the most discriminating questions
mkQuestion :: [[String]] -> [Question]
mkQuestion = transpose .
  differingItems
  
differingItems :: (Ord a) => [[a]] -> [[a]]
differingItems = filter (not . allSame) . transpose . map sort

-- >>> gr <- getPGF
-- >>> mkQuestion $ extractContentWords (parsePred gr "SoleIndependentContractor")
-- [["independent_A","sole_V2"],["independent_N","sole_V2"],["independent_A","sole_A"],["independent_N","sole_A"],["independent_A","sole_N"],["independent_N","sole_N"],["independent_N","sole_N"]]

extractContentWords :: Predicate -> [[String]]
extractContentWords pr = [onlyLex (fg' tree) | tree <- trees pr]
  where
    fg' :: Expr -> GFullPredicate
    fg' = fg

    onlyLex :: Tree a -> [String]
    onlyLex tree = case fromLexicalNode tree of
      Just s -> [s]
      Nothing -> composOpMonoid onlyLex tree


-- >>> gr <- getPGF
-- >>> (parsePred gr "SoleIndependentContractor")

parsePred :: PGF -> [Char] -> Predicate
parsePred gr str = Pred nm (filterHeuristic ts) ar
  where
    nm : _ = splitOn ":" str
    ar = length $ filter (== '>') str
    ts = parseGF gr nm

type MyParseOutput = [Expr]

parseGF :: PGF -> String -> MyParseOutput
parseGF gr str = go wds
  where
    wds = map (trim . map toLower) $ split (startsWithOneOf (['A' .. 'Z']++['0'..'9']) ) str
    lang = head $ languages gr
    cat = startCat gr
    go :: [String] -> MyParseOutput
    go ws = finalParse
      where
        (output, bstring) = parse_ gr lang cat Nothing (unwords ws)
        finalParse = case output of
          ParseOk ts -> ts
          ParseFailed n | all isLower (ws !! (n-1)) ->
            go
              [ capWd
                | (ind, w:ord) <- zip [1..] ws,
                  let capWd
                        | ind == n = toUpper w : ord
                        | otherwise = w:ord
              ]
          ParseFailed 1 -> [gf GNoParse]
          ParseFailed n -> go (take (n-1) ws) ++ [gf (GParseFailedAfterNTokens (GInt n))]
          ParseIncomplete -> go (init ws)
          _ -> []

-- if we want [(Expr,BracketedString)]
-- filterHeuristic' :: MyParseOutput -> MyParseOutput
-- filterHeuristic' ps = [(t,b) | (t,b) <- ps, not $ ppBeforeAP t]

filterHeuristic :: [Expr] -> [Expr]
filterHeuristic ts = filter (not . ppBeforeAP) (filter filterGerund ts)
  where
    filterGerund
      | any hasGerund ts &&  -- "practicing as lawyer": progressive, pres. part. or gerund
        any hasProgr ts && 
        not (all hasGerund ts) = not . hasProgr
--      | any hasGerund ts && not (all hasGerund ts) = hasGerund
      | otherwise = const True

-- TODO only remove apposition, keep compound noun


hasGerund :: Expr -> Bool
hasGerund = getAny . hasGerund' . (fg :: Expr -> GFullPredicate)
  where
    hasGerund' :: Tree a -> Any
    hasGerund' (GGerundCN _) = Any True
    hasGerund' x = composOpMonoid hasGerund' x

hasProgr :: Expr -> Bool
hasProgr = getAny . hasProgr' . (fg :: Expr -> GFullPredicate)
  where
    hasProgr' :: Tree a -> Any
    hasProgr' (GProgrVP _) = Any True
    hasProgr' (GUseComp (GCompAP (GPresPartAP _))) = Any True
    hasProgr' x = composOpMonoid hasProgr' x

ppBeforeAP :: Expr -> Bool
ppBeforeAP = getAny . ppBeforeAP' . (fg :: Expr -> GFullPredicate)
  where
    ppBeforeAP' :: Tree a -> Any
    ppBeforeAP' (GAdjCN (GPastPartAP _) (GAdjCN _ _)) = Any True
    ppBeforeAP' (GAdjCN (GPastPartAgentAP _ _) (GAdjCN _ _)) = Any True
    ppBeforeAP' x = composOpMonoid ppBeforeAP' x
