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
import Data.Maybe (isJust, mapMaybe)
import UDConcepts ( UDSentence, prUDSentence, prReducedUDSentence, prQuickUDSentence )
import UDAnnotations (getEnv, initUDEnv, UDEnv (..))
import GF2UD
import Debug.Trace (trace)
import qualified Data.Map as M
import Control.Arrow (Arrow (first, second))
import Data.Tuple (swap)
import qualified Data.Bifunctor as Bifunctor
import Control.Exception (try)
import Control.Monad (zipWithM)

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

data Some f = forall a. Some (f a)

toLexicalNode :: String -> Maybe (Some Tree)
toLexicalNode str =
  case last $ splitOn "_" str of
    "A"      -> Just $ Some $ LexA str
    "A2"     -> Just $ Some $ LexA2 str
    "AdV"    -> Just $ Some $ LexAdV str
    "Adv"    -> Just $ Some $ LexAdv str
    "Card"   -> Just $ Some $ LexCard str
    "Det"    -> Just $ Some $ LexDet str
    "N"      -> Just $ Some $ LexN str
    "N2"     -> Just $ Some $ LexN2 str
    "PN"     -> Just $ Some $ LexPN str
    "Predet" -> Just $ Some $ LexPredet str
    "Prep"   -> Just $ Some $ LexPrep str
    "Pron"   -> Just $ Some $ LexPron str
    "Quant"  -> Just $ Some $ LexQuant str
    "Subj"   -> Just $ Some $ LexSubj str
    "V"      -> Just $ Some $ LexV str
    "V2"     -> Just $ Some $ LexV2 str
    "V2A"    -> Just $ Some $ LexV2A str
    "V2S"    -> Just $ Some $ LexV2S str
    "V2V"    -> Just $ Some $ LexV2V str
    "V3"     -> Just $ Some $ LexV3 str
    "VA"     -> Just $ Some $ LexVA str
    "VS"     -> Just $ Some $ LexVS str
    "VV"     -> Just $ Some $ LexVV str
    "N3"     -> Just $ Some $ LexN3 str
    "Text"   -> Just $ Some $ LexText str
    "V2Q"    -> Just $ Some $ LexV2Q str
    _ -> Nothing

----------------------------------------------------

type ReducedUDSentence a = [ReducedUDWord a]
data ReducedUDWord a = RUDW Int String a -- where a is an instance of Gf
  deriving (Eq, Ord, Show, Read)

data Choice a = Whatever | Exactly a
  deriving (Eq, Ord, Show, Read)

matches :: Eq a => a -> Choice a -> Bool
matches a Whatever = True
matches a (Exactly b) = a == b

-- | A partial disambiguation of a sentence
type Constraint = ReducedUDWord (Choice String)
type Constraints = ReducedUDSentence (Choice String)

-- TODO: Proper error handling and prettier format
parseConstraints :: String -> Constraints
parseConstraints = map read . lines

serializeConstraints :: Constraints -> String
serializeConstraints = unlines . map show

writeConstraints :: FilePath -> Constraints -> IO ()
writeConstraints fp = writeFile fp . serializeConstraints

readConstraints :: FilePath  -> IO (Maybe Constraints)
readConstraints = (fmap.fmap) parseConstraints . tryReadFile

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
  result <- try $ readFile fp :: IO (Either IOError String)
  pure $ either (const Nothing) Just result

matchesConstraint :: ReducedUDWord String -> Constraint -> Maybe Bool
matchesConstraint (RUDW i wf fun) (RUDW i' wf' fun')
  | i==i' && wf==wf' = Just $ matches fun fun'
  | otherwise        = Nothing

matchesConstraints :: ReducedUDSentence String -> Constraints -> Maybe Bool
matchesConstraints s c = and <$> zipWithM matchesConstraint s c

parseRUDW :: UDSentence -> ReducedUDSentence String
parseRUDW uds = parseUDW <$> lines str
  where
    str = prReducedUDSentence "xx_______x" uds
    parseUDW wrd =
      case splitOn "\t" wrd of
        [n,nm,'F':'U':'N':'=':fun] -> RUDW (read n) nm fun


showMPO :: MyParseOutput -> String
showMPO (udt,t) = unlines [showExpr [] t, prReducedUDSentence "xx_______x" udt]

{--- TODO:
* Print out these into a .l4lex file
* Ask user about individual words, filter out trees based on answers

Plan:

Given a list of constraints
Take list of ReducedUDSentences with GfTrees
Filter out those that matches the constraints

Choose an arbitrary word (or the one with the most choices) (or the first)
Filter out unique choices for that word (possibly saving a map to what sentences each choice belongs to)
if it has at least two choices:
  Ask the user which of the word variants to use
  Save that variant to the set of constraints
if all are unique
  Get the parse from the map
  (Optional: Save the inferred words in constraints file)


-}
----------------------------------------------------

type Arity = Int

-- | 
data Predicate
  = Pred
      { name :: String              -- ^ DetractsFromDignity
      , description :: String       -- ^ "detracts from dignity of legal profession"
      , trees :: [MyParseOutput]    -- ^ A list of all possible parses
      , reducedUDmap :: M.Map (ReducedUDSentence String) [Expr]
      , arity :: Arity              -- ^ Arity of the predicate, e.g. DetractsFromDignity : Business -> Bool, arity=1
      }

instance Show Predicate where
  show (Pred nm _ [] _ ar) = printf "%s\narity %d, no parses" nm ar
  show (Pred nm _ ts _ ar) = printf "%s\narity %d\nparses\n%s" nm ar exprs
    where
      exprs = unlines $ map showMPO ts

type Name = String
type Description = [String]

parsePred :: UDEnv -> Arity -> Name -> String -> Predicate
parsePred udenv ar funname optdesc = trace
   ("ts pre-filter: " ++ unlines (map showMPO ts) ++ "\n" ++
    "arity: " ++ show ar ++ ", ts post-filter: " ++ unlines (map showMPO mpo))
    $
    Pred
      nm
      (unwords desc)
      mpo
      (mkMap $ (map.first) parseRUDW mpo)
      ar
  where
    mpo = filterHeuristic ar ts
    nm : _ = splitOn ":" funname
    nm' = unwords $ splitOn "_" nm -- take care of is_participant_in style
    desc = case optdesc of -- if no description provided, parse the name, e.g. DescribedInSection1
              [] -> map (trim . lower) $ split capsOrDigits nm'
              _  -> words optdesc
    ts = filter (not . hasMeta . snd) (parseGF udenv desc)
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

mkMap :: (Ord k) => [(k, v)] -> M.Map k [v]
mkMap = M.fromListWith (<>) . map (second pure)

{- HLINT ignore expr2ud "Eta reduce" -}
expr2ud :: UDEnv -> Expr -> UDSentence
expr2ud udenv expr = gf2ud udenv lang expr
  where
    lang = head $ languages $ pgfGrammar udenv

type MyParseOutput = (UDSentence, Expr)

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
          ParseOk ts -> [(expr2ud udenv t, t) | t <- ts]
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
filterHeuristic ar ts_udts = [ (udt, extractLex t)
                        | (udt, t) <- ts_udts
                        , not $ ppBeforeAP t
                        , filterGerund t
                        , filterArity t ]
  where
    ts = map snd ts_udts
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
