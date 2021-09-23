{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DeriveTraversable #-}
module ToGF.ParsePred where


import ParsePredicates as PP
import Data.Char (toLower, isUpper)
import Data.List.Extra (trim, transpose, allSame)
import Data.List.Split ( split, splitOn, startsWithOneOf )
import PGF hiding (Tree)
import Text.Printf (printf)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import UDConcepts ( UDSentence, prReducedUDSentence )
import UDAnnotations (UDEnv (..))
import GF2UD
import ToGF.Disambiguate
import qualified Data.Map as M
import Control.Arrow (Arrow (first))
import Control.Exception (try)
import Control.Monad (mfilter, forM_)
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Foldable (for_)
import System.IO (stdout, hFlush)

-- import Debug.Trace (trace)

-- TODO: Use logging flags to determine if trace should be used
trace :: p1 -> p2 -> p2
trace _ a = a

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

rudwWord :: ReducedUDWord a -> String
rudwWord (RUDW _ wrd _) = wrd

forgetContents :: Functor f => f a -> f ()
forgetContents x = () <$ x

sameStructure :: (Eq (f ()), Functor f) => f a -> f b -> Bool
sameStructure a b = forgetContents a == forgetContents b

linearizeRUDS :: ReducedUDSentence a -> String
linearizeRUDS = unwords . map rudwWord . unRUDS

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
matches _ Whatever = True
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

createEmptyConstraints :: UDSentenceMap -> Maybe Constraints
createEmptyConstraints = fmap (Whatever <$) . listToMaybe . M.keys

parseRUDW :: UDSentence -> ReducedUDSentence String
parseRUDW uds = RUDS $ parseUDW <$> lines str
  where
    str = prReducedUDSentence "xx_______x" uds
    parseUDW wrd =
      case splitOn "\t" wrd of
        [n,nm,'F':'U':'N':'=':fun] -> RUDW (read n) nm fun
        _ -> error $ wrd ++ " doesn't match expected shape"


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

mkFilename :: Predicate -> FilePath -> FilePath
mkFilename prd filePrefix = filePrefix ++ name prd ++ ".l4lex"

-- TODO: This throws away all old data if the sentence has changed at all
-- Do we want to try to preserve some data so the user won't have to answer the
-- same questions again and again regardless of how small the change?
getAndValidateConstraints :: Predicate -> FilePath -> IO (Maybe Constraints)
getAndValidateConstraints prd filePrefix = do
  ctrs <- readConstraints $ mkFilename prd filePrefix
  pure $ mfilter (validateConstraints (reducedUDmap prd)) ctrs

-- TODO: Put multiple predicates in a single file.
saveConstraints :: Predicate -> FilePath -> Constraints -> IO ()
saveConstraints prd filePrefix ctrs = do
  writeConstraints (mkFilename prd filePrefix) ctrs

askConstraint :: Predicate -> Constraints -> IO (Maybe Constraints)
askConstraint prd ctrs = do
  -- TODO: Extract pure code
  let relevantUDs = filterMatching ctrs (reducedUDmap prd)
  let nextQuestion = getNextQuestion $ M.keys relevantUDs
  whenJust nextQuestion $ \ q@(RUDW _ wrd alts) -> do
      putStrLn $ "In the predicate: " ++ show (description prd)
      putStrLn $ "How should " ++ show wrd ++ " be interpreted?"
      forM_ alts $ \alt -> putStrLn $ " - " ++ alt
      putStr "> "
      hFlush stdout
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
      | otherwise = ctr

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
  let ctrs = fromMaybe (fromMaybe (error $ "No words in ud map\n" ++ show (reducedUDmap prd) ++ "\nfor\n" ++ show prd) . createEmptyConstraints $ reducedUDmap prd) mbCtrs

  newCtrs <- askConstraint prd ctrs
  whenJust_ newCtrs $ saveConstraints prd filePrefix
  pure $ fromMaybe ctrs newCtrs

askUntilUnambigous :: Predicate -> FilePath -> IO [Expr]
askUntilUnambigous prd filePrefix = do
  ctrs <- askAboutPredicate prd filePrefix
  case M.toList $ filterMatching ctrs (reducedUDmap prd) of
    [] -> error $ "BUG: Couldn't find any possibilities\n" ++ show prd ++ "\n" ++ show ctrs
    [(_,exprs)] -> pure exprs
    _ -> askUntilUnambigous prd filePrefix

-- createQuestiosn :: MyParseOutput -> Map Question MyParseOutput
-- Creates questions using another grammar based on MyparseOutput

-- askStuff :: Map Question MyParseOutput -> IO MyParseOutput
-- less trees than in the original MPO

filterPredicate :: UDEnv -> FilePath -> Predicate -> IO Predicate
filterPredicate udenv filePrefix prd = do
  let _qmap = mkQuestionMap (pgfGrammar udenv) $ mkQuestions (trees prd)
  -- TODO: Use a proper logging system that allows outputting this depending on flags
  -- TODO: Actually use this to ask questions
  -- putStrLn $ showQuestionMap qmap
  exprs <- askUntilUnambigous prd filePrefix
  pure $ prd { trees = exprs}

----------------------------------------------------

type Arity = Int

-- |
data Predicate
  = Pred
      { name :: String              -- ^ DetractsFromDignity
      , description :: String       -- ^ "detracts from dignity of legal profession"
      , trees :: [Expr]   -- ^ A list of all possible parses
      , reducedUDmap :: UDSentenceMap
      , arity :: Arity              -- ^ Arity of the predicate, e.g. DetractsFromDignity : Business -> Bool, arity=1
      }

instance Show Predicate where
  show (Pred nm _ [] _ ar) = printf "%s\narity %d, no parses" nm ar
  show (Pred nm _ ts _ ar) = printf "%s\narity %d\nparses\n%s" nm ar exprs
    where
      exprs = unlines $ map (showExpr []) ts

type Name = String
type Description = [String]

filterValidRuds :: [String] -> [(ReducedUDSentence String, Expr)] -> [(ReducedUDSentence String, Expr)]
filterValidRuds desc = filter $ (== unwords desc) . linearizeRUDS . fst

validatePredicate :: Predicate -> Predicate
validatePredicate prd
  | Just rud <- L.find ((/= description prd) . linearizeRUDS) $ pred2rudss prd  = error $ "Error: " ++ show rud ++ " doesn't match " ++ show (description prd)
  | otherwise = prd

pred2rudss :: Predicate -> [ReducedUDSentence String]
pred2rudss = M.keys . reducedUDmap

parsePred :: UDEnv -> Arity -> Name -> String -> Predicate
parsePred udenv ar funname optdesc = validatePredicate $ trace
   ("arity: " ++ show ar ++
    "\nts pre-filter: " ++ unlines pre_filter ++
    "\nts post-filter: " ++ unlines (map showMPO filtered_uds_ts))
    $
    Pred
      nm
      (unwords desc)
      (map snd filtered_uds_ts)
      (mkMap $ filterValidRuds desc $ (map.first) parseRUDW filtered_uds_ts)
      ar
  where
    pre_filter = map showMPO uds_ts L.\\ map showMPO filtered_uds_ts
    filtered_uds_ts = filterHeuristic ar uds_ts
    nm : _ = splitOn ":" funname
    nm' = nm
    --nm' = unwords $ splitOn "_" nm -- if we want to take care of is_participant_in style
    desc = case optdesc of -- if no description provided, parse the name, e.g. DescribedInSection1
              [] -> map (trim . lower) $ split capsOrDigits nm'
              _  -> words optdesc
    uds_ts = filter (not . hasMeta . snd) (parseGF udenv desc)
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
        (output, _bstring) = parse_ gr lang cat Nothing (unwords ws)
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
