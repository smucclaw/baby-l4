{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DeriveTraversable #-}
module ToGF.ParsePred where


import ParsePredicates as PP
import Data.Char (toLower, isUpper)
import Data.List.Extra (trim, transpose, allSame)
import Data.List.Split ( split, splitOn, startsWithOneOf )
import Data.Monoid (Any (..))
import PGF hiding (Tree)
import Text.Printf (printf)
import Data.Maybe (isJust, fromMaybe)
import UDConcepts ( UDSentence, prReducedUDSentence )
import UDAnnotations (UDEnv (..))
import GF2UD
import Debug.Trace (trace)
import qualified Data.Map as M
import Control.Arrow (Arrow (first, second))
import Control.Exception (try)
import Control.Monad (mfilter)
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Foldable (for_)

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

newtype ReducedUDSentence a = RUDS [ReducedUDWord a]
  deriving (Eq, Ord, Show, Read, Functor)

unRUDS :: ReducedUDSentence a -> [ReducedUDWord a]
unRUDS (RUDS x) = x

data ReducedUDWord a = RUDW Int String a -- where a is an instance of Gf
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

unRUDW :: ReducedUDWord a -> a
unRUDW (RUDW _ _ x) = x

forgetContents :: Functor f => f a -> f ()
forgetContents x = () <$ x

sameStructure :: (Eq (f ()), Functor f) => f a -> f b -> Bool
sameStructure a b = forgetContents a == forgetContents b

-- INVARIANT: All ReducedUDSentences must correspond to the same sentence
type UDSentenceMap = M.Map (ReducedUDSentence String) [Expr]

validUDSentenceMap :: UDSentenceMap -> Bool
validUDSentenceMap = allSame . map forgetContents . M.keys

-- | sameSentence a b returns true if both corresponds to (potentially different interpretations of) the same sentence
sameSentence :: ReducedUDSentence a -> ReducedUDSentence b -> Bool
sameSentence = sameStructure

-- | sameSentence a b returns true if both corresponds to (potentially different interpretations of) the same word
sameWord :: ReducedUDWord a -> ReducedUDWord b -> Bool
sameWord = sameStructure

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
parseConstraints = RUDS . map read . lines

serializeConstraints :: Constraints -> String
serializeConstraints = unlines . map show . unRUDS

writeConstraints :: FilePath -> Constraints -> IO ()
writeConstraints fp = writeFile fp . serializeConstraints

readConstraints :: FilePath  -> IO (Maybe Constraints)
readConstraints = (fmap.fmap) parseConstraints . tryReadFile

matchesConstraint :: ReducedUDWord String -> Constraint -> Bool
matchesConstraint (RUDW i wf fun) (RUDW i' wf' fun')
  | i==i' && wf==wf' = matches fun fun'
  | otherwise        = error $ "BUG: " ++ show wf ++ " is not the same word as " ++ show wf'
-- matchesConstraint = unRUDW $ liftA2 matches

-- TODO: Use zipWithExact instead
matchesConstraints :: ReducedUDSentence String -> Constraints -> Bool
matchesConstraints s c = and $ zipWith matchesConstraint (unRUDS s) (unRUDS c)
-- matchesConstraints = unRUDS $ liftA2 matches

filterMatching :: Constraints -> UDSentenceMap -> UDSentenceMap
filterMatching constrs = M.filterWithKey (\ k _ -> matchesConstraints k constrs)

createEmptyConstraints :: UDSentenceMap -> Constraints
createEmptyConstraints = (Whatever <$) . head . M.keys

parseRUDW :: UDSentence -> ReducedUDSentence String
parseRUDW uds = RUDS $ parseUDW <$> lines str
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

-- | Check that constraints are for the current sentence
validateConstraints :: UDSentenceMap -> Constraints -> Bool
validateConstraints = sameSentence . head . M.keys

-- TODO: This throws away all old data if the sentence has changed at all
-- Do we want to try to preserve some data so the user won't have to answer the
-- same questions again and again regardless of how small the change?
getAndValidateConstraints :: Predicate -> FilePath -> IO (Maybe Constraints)
getAndValidateConstraints prd filePrefix = do
  ctrs <- readConstraints $ filePrefix ++ name prd ++ "lex"
  pure $ mfilter (validateConstraints (reducedUDmap prd)) ctrs

-- TODO: Put multiple predicates in a single file.
saveConstraints :: Predicate -> FilePath -> Constraints -> IO ()
saveConstraints prd filePrefix ctrs = do
  writeConstraints (filePrefix ++ name prd ++ "lex") ctrs

askConstraint :: Predicate -> Constraints -> IO (Maybe Constraints)
askConstraint prd ctrs = do
  -- TODO: Extract pure code
  let relevantUDs = filterMatching ctrs (reducedUDmap prd)
  let nextQuestion = getNextQuestion $ M.keys relevantUDs
  whenJust nextQuestion $ \ q -> do
      putStrLn $ "Which one: " ++ show q
      putStr "> "
      -- TODO: Make this more flexible
      x <- getLine
      let matching = traverse (L.find (== x)) q
      let ctrs' = fmap (`updateCtrs` ctrs) matching
      pure ctrs'

updateCtrs :: ReducedUDWord String -> Constraints -> Constraints
updateCtrs val = RUDS . map updateCtr . unRUDS
  where
    updateCtr :: Constraint -> Constraint
    updateCtr ctr
      | sameWord val ctr = Exactly (unRUDW val) <$ ctr
      | otherwise = error $ "Trying to update " ++ show ctr ++ " to " ++ show val

getNextQuestion :: [ReducedUDSentence String] -> Maybe (ReducedUDWord [String])
getNextQuestion = firstAmbigous . fmap sortNub . transposeSentence
  where

  -- | Given a list of sentences, match up the corresponding ones with each other
  transposeSentence :: [ReducedUDSentence String] -> ReducedUDSentence [String]
  transposeSentence = RUDS . map transposeWord . transpose . map unRUDS
  -- transposeSentence = sequenceA

  -- | Given a list of words, check that they all have the same key and merge them1
  transposeWord :: [ReducedUDWord String] -> ReducedUDWord [String]
  transposeWord wds
    | allSameWord wds = map unRUDW wds <$ head wds
    | otherwise = error $ "Mismatch between words: " ++ show wds

  allSameWord :: [ReducedUDWord a] -> Bool
  allSameWord = allSame . map forgetContents

  firstAmbigous :: ReducedUDSentence [a] -> Maybe (ReducedUDWord [a])
  firstAmbigous = L.find ((>= 2) . length . unRUDW) . unRUDS

untransposeWord :: ReducedUDWord [a] -> [ReducedUDWord a]
untransposeWord (RUDW i w x) = RUDW i w <$> x
-- untransposeWord = sequenceA

-- | Read constraints from a file, ask about new constraints and save the changes.
askAboutPredicate :: Predicate -> FilePath -> IO Constraints
askAboutPredicate prd filePrefix = do
  mbCtrs <- getAndValidateConstraints prd filePrefix
  let ctrs = fromMaybe (createEmptyConstraints $ reducedUDmap prd) mbCtrs

  newCtrs <- askConstraint prd ctrs
  whenJust_ newCtrs $ saveConstraints prd filePrefix
  pure $ fromMaybe ctrs newCtrs

----------------------------------------------------

type Arity = Int

-- |
data Predicate
  = Pred
      { name :: String              -- ^ DetractsFromDignity
      , description :: String       -- ^ "detracts from dignity of legal profession"
      , trees :: [MyParseOutput]    -- ^ A list of all possible parses
      , reducedUDmap :: UDSentenceMap
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

--------

-- Misc helpers

-- | Sort a list and throw away duplicates
sortNub :: Ord a => [a] -> [a]
sortNub = Set.toList . Set.fromList

-- | Read a file and catch any thrown errors
tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
  result <- try $ readFile fp :: IO (Either IOError String)
  pure $ either (const Nothing) Just result

whenJust_ :: Applicative f => Maybe t -> (t -> f ()) -> f ()
whenJust_ = for_

whenJust :: Applicative f => Maybe t -> (t -> f (Maybe a)) -> f (Maybe a)
whenJust a f = case a of
  Nothing -> pure Nothing
  Just q -> f q
