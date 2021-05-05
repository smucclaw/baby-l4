{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.GenerateLexicon where

import qualified Data.Set as S
import qualified GF
import PGF (PGF, Expr, readPGF, linearizeAll, showExpr)
import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)
import ToGF.ParsePred
import ToGF.FromSCasp.SCasp as SC hiding (parens)
import System.Environment (withArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)
import Data.List.Extra (splitOn, trim, intercalate)
import Data.Set (Set)
import Syntax (Mapping(..))

----------------------------------------------------------------------
-- Generate GF code

indent' :: Doc ann -> Doc ann
indent' = indent 4

type GrName = String

topName, lexName :: GrName -> Doc () --String
topName grName = pretty grName <> "Top"
lexName grName = pretty grName <> "Lexicon"

mkAbsName, mkCncName, mkPGFName :: Doc () -> String
mkAbsName d = printf "grammars/%s.gf" (show d)
mkCncName d = printf "grammars/%sEng.gf" (show d)
mkPGFName d = printf "generated/%s.pgf" (show d)

createPGF :: Doc () -> IO PGF.PGF
createPGF nm = do
  withArgs
    [ "-make",
      "--output-dir=generated",
      "--gfo-dir=generated",
      "-v=0",
      mkCncName nm
    ]
    GF.main
  PGF.readPGF $ mkPGFName nm

createGF' :: GrName -> [Mapping t] -> [AtomWithArity] -> IO PGF.PGF
createGF' gname userlexicon model = do
  let lName = lexName gname
      tName = topName gname
      grName = pretty gname
--  parsepgf <- readPGF =<< getDataFileName "ParsePredicates.pgf"
  parsepgf <- readPGF "generated/ParsePredicates.pgf"
  let (absS, cncS) = mkLexicon parsepgf gname userlexicon model
  writeDoc (mkAbsName lName) absS
  writeDoc (mkCncName lName) cncS
  writeDoc (mkAbsName tName) $ "abstract" <+> tName <+> "=" <+> grName <> "," <+> lName <+> "** {flags startcat = Statement ;}"
  writeDoc (mkCncName tName) $ "concrete" <+> tName <> "Eng of " <> tName <+> "=" <+> grName <> "Eng," <+> lName <> "Eng ;"
  createPGF tName

writeDoc :: FilePath -> Doc () -> IO ()
writeDoc nm doc = withFile nm WriteMode $ \h -> hPutDoc h doc

mkLexicon :: PGF -> GrName -> [Mapping t] -> [AtomWithArity] -> (Doc (), Doc ())
mkLexicon parsepgf gname userlex atoms = (abstractLexicon gname parsedLex guessedLex, concreteLexicon gname parsedLex guessedLex)
  where
    bothLexica =
      [ ( parsePredFromUserLex funname
          ++ parsePredFromName funname
        , guessPOS aa)
      | aa@(AA funname _) <- atoms
      ]
    parsePredFromUserLex funname = [ pr -- only those user-defined predicates that actually parse!
                                   | Mapping _ nm value <- userlex
                                   , nm == funname
                                   , let pr = parsePred parsepgf nm value
                                   , not $ null $ trees pr -- TODO: why doesn't this work?
                                   ] -- is empty if the funname doesn't appear in user lex, or if there's no parse
    parsePredFromName funname = [ pr
                                | let pr = parsePred parsepgf funname ""
                                , not $ null $ trees pr ]
    parsedLex = [ p | (p:_ , _) <- bothLexica] -- Use the parsed predicate. TODO filter in smart way, not just use the first one!
    guessedLex = [ posguess | ([] , posguess) <- bothLexica] -- If no result for parsePred, fall back to guessed pos

printGF' :: PGF -> Expr -> IO ()
printGF' gr expr = do
  --putStrLn $ showExpr [] expr
  mapM_ (putStrLn . postprocess) (linearizeAll gr expr)

postprocess :: String -> String
postprocess = map (\c -> if c == '\\' then '\n' else c)


----------------------------------------------------------------------
-- If there is no lexicon available, we parse the predicates and use GF smart paradigms

-- Internal format that works for all sources
data AtomWithArity = AA String Int deriving (Show, Eq, Ord)

getAtoms :: SC.Tree s -> Set AtomWithArity
getAtoms = SC.foldMapTree getAtom
  where
    getAtom :: SC.Tree a -> Set AtomWithArity
    getAtom (EApp (A str) ts) = S.singleton $ AA str (length ts)
    getAtom (AAtom (A str))   = S.singleton $ AA str 0
    getAtom _                 = mempty

-- POS
type Prep = Maybe String

data POS = POS {origName :: String, pos :: InnerPOS} deriving (Show, Eq)

data InnerPOS = PN2 String Prep | PN String | PV2 String Prep | PV String | PGuess String deriving (Show,Eq)

guessPOS :: AtomWithArity -> POS
guessPOS (AA str int) = POS str $
  case (int, splitOn "_" str) of
  (0, [noun]) -> PN noun
  (2, ["is", noun, prep]) -> PN2 noun (Just prep) -- e.g. is_participant_in
  (2, ["is", noun]) -> PN2 noun Nothing           -- e.g. is_winner
  (1, ["is", noun]) -> PN noun                    -- e.g. is_game
  (1, ["is", noun, _]) -> PN noun                 -- for completeness' sake
  (1, [verb]) -> PV verb
  (2, [verb]) -> PV2 verb Nothing
  (2, [verb, prep]) -> PV2 verb (Just prep)
  _ -> PGuess str


concreteLexicon :: GrName -> [Predicate] -> [POS] -> Doc ()
concreteLexicon gname userlexicon poses = let lName = lexName gname in
  vsep
    [ "concrete" <+> lName <> "Eng of" <+> lName <+> "=" <+> "AtomsEng ** open PredicatesEng, SyntaxEng, ParadigmsEng, AdjectiveEng, ReducedWordNetEng in {",
      "lin",
      (indent' . vsep) (concrEntryPOS <$> poses),
      (indent' . vsep) (concrEntryUserLex <$> userlexicon),
      "oper",
      "    p1 : {pred : VPS} -> LinAtom = \\vps -> mkAtom <vps.pred : VPS> ;",
      "    p2 : {pred : VPS2} -> LinAtom = \\vps2 -> mkAtom <vps2.pred : VPS2> ;",
      "}"
    ]

abstractLexicon :: GrName -> [Predicate] -> [POS] -> Doc ()
abstractLexicon gname userlexicon poses =
  vsep
    [ "abstract" <+> lexName gname <+> "=" <+> "Atoms ** {",
      "fun",
      indent' $ sep $ punctuate "," $ map pretty
        (map origName poses ++ map name userlexicon),
      indent' ": Atom ;",
      "}"
    ]

concrEntryPOS :: POS -> Doc ()
concrEntryPOS (POS nm p) = hsep [pretty nm, "=", "mkAtom", parens $ innerLex p, ";"]
  where
    innerLex :: InnerPOS -> Doc ()
    innerLex (PN2 n pr) =
      "mkN2" <+> parens (innerLex (PN n))
        <+> case pr of
          Nothing -> "possess_Prep"
          Just prep -> "(mkPrep \"" <> pretty prep <> "\")"
    innerLex (PN n) = "mkN" <+> viaShow n

    innerLex (PV2 v pr) = "mkV2" <+> parens (innerLex (PV v))
      <+> case pr of
        Nothing -> ""
        Just prep -> pretty prep <> "_Prep"
    innerLex (PV v) = "mkV" <+> viaShow v
    innerLex (PGuess np) = let invar = viaShow np in "mkN" <+> invar <+> invar

concrEntryUserLex :: Predicate -> Doc ()
concrEntryUserLex pr =
  case trees pr of
    [] -> mempty
    t:_ -> hsep $ map pretty [name pr, "=", hackyRemoveFullPred $ showExpr [] t, ";"]

-- TODO: handle this function as Gf trees to other Gf trees, not string processing
hackyRemoveFullPred :: String -> String
hackyRemoveFullPred str = case words $ hackyChangeIntToCard $ trim str of
                       "FullPred":ws -> unwords ws
                       "PredAP":_pol:ws -> printf "p1 (ComplAP %s)" $ unwords ws
                       "V2PartAdv":_pol:v2:adv
                         -> printf "p1 (ComplAP (AdvAP (PastPartAP (mkVPSlash %s)) %s))" v2 (unwords adv)
                       _ -> "mkAtom business_N"

hackyChangeIntToCard :: String -> String
hackyChangeIntToCard str = case splitOn "(Int2Card 1)" str of
                        [] -> str
                        xs -> intercalate "(mkCard \"1\")" xs



--- TODO: filter out predicates based on arity