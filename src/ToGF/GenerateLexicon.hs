{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.GenerateLexicon where

import qualified GF
import PGF (PGF, Expr, readPGF, linearizeAll, showExpr)
import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)
import ToGF.ParsePred
import System.Environment (withArgs, getEnv)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)
import Data.List.Extra (splitOn, trim, intercalate)
import Syntax (Mapping(..))
import Data.Maybe (listToMaybe)
import Control.Applicative ((<|>))
import Paths_baby_l4 (getDataFileName)
import qualified UDAnnotations as UDA
import System.Directory.Extra (createDirectoryIfMissing, copyFile)

----------------------------------------------------------------------
-- Generate GF code

indent' :: Doc ann -> Doc ann
indent' = indent 4

type GrName = String

generatedFileDir :: String
generatedFileDir = ".l4-generated"

topName, lexName :: GrName -> String
topName grName = grName <> "Top"
lexName grName = grName <> "Lexicon"

mkGeneratedFileName :: String -> String
mkGeneratedFileName d = generatedFileDir <> "/" <> d

mkAbsName, mkCncName, mkPGFName :: String -> String
mkAbsName d = d <> ".gf"
mkCncName d = d <> "Eng.gf"
mkPGFName d = d <> ".pgf"

createPGF :: String -> IO PGF.PGF
createPGF nm = do
  oldLibPath <- getEnv "GF_LIB_PATH"
  grammarsDir <- getDataFileName "grammars"
  let libPath = grammarsDir <> ":" <> oldLibPath

  withArgs
    [ "-make",
      "--gf-lib-path=" <> libPath,
      "--output-dir="<> generatedFileDir,
      "--gfo-dir="<> generatedFileDir,
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
      writeGenerated = writeDoc . mkGeneratedFileName
  parsepgfName <- getDataFileName "grammars/ParsePredicates"
  udenv <- UDA.getEnv parsepgfName "Eng" "Predicate"
  let (absS, cncS) = mkLexicon udenv gname userlexicon model
  createDirectoryIfMissing False generatedFileDir
  -- Hack to make sure a modern version of RGL exists
  dataDir <- getDataFileName "grammars"
  copyFile (dataDir <> "/ExtendEng.gfo") (mkGeneratedFileName "ExtendEng.gfo")
  writeGenerated (mkAbsName lName) absS
  writeGenerated (mkCncName lName) cncS
  writeGenerated (mkAbsName tName) $
    "abstract" <+> pretty tName <+> "=" <+> grName <> "," <+> pretty lName <+> "** {flags startcat = Statement ;}"
  writeGenerated (mkCncName tName) $
    "concrete" <+> pretty tName <> "Eng of " <> pretty tName <+> "=" <+> grName <> "Eng," <+> pretty lName <> "Eng ;"
  createPGF $ mkGeneratedFileName tName

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc nm doc = withFile nm WriteMode $ \h -> hPutDoc h doc

mkLexicon :: UDA.UDEnv -> GrName -> [Mapping t] -> [AtomWithArity] -> (Doc (), Doc ())
mkLexicon udenv gname userlex atoms =
  (abstractLexicon gname parsedLex guessedLex,
   concreteLexicon gname parsedLex guessedLex)
  where
    bothLexica =
      [ ( parsePredFromUserLex funname ar'
          <|> parsePredFromName funname ar'
        , guessPOS aa)
      | aa@(AA funname ar) <- atoms
      , let ar' = ar --max ar (length $ filter (=='>') funname) -- TODO see if these are ever different?
      ]
    parsePredFromUserLex funnm ar = listToMaybe [ pr
                                   | Mapping _ nm value <- userlex
                                   , nm == funnm
                                   , let pr = parsePred udenv ar nm value
                                   , not $ null $ trees pr ] -- is empty if the funnm doesn't appear in user lex, or if there's no parse
    parsePredFromName funnm ar = listToMaybe [ pr
                                 | let pr = parsePred udenv ar funnm ""
                                 , not $ null $ trees pr ]
    parsedLex = [ parsed | (Just parsed , _) <- bothLexica] -- Use the parsed predicate.
    guessedLex = [ guess | (Nothing , guess) <- bothLexica] -- If no result for parsePred, fall back to guessed pos

printGF' :: PGF -> Expr -> IO ()
printGF' gr expr = do
  --putStrLn $ showExpr [] expr
  mapM_ (putStrLn . postprocess) (linearizeAll gr expr)

postprocess :: String -> String
postprocess = map (\c -> if c == '\\' then '\n' else c)

-- TODO
-- Generating & copying PGF file to right place
-- more filtering of new GF funs
-- generalise subject in PredSentence*
-- Remove int2card, replace with custom digits, incl. decimals

----------------------------------------------------------------------
-- If there is no lexicon available, we parse the predicates and use GF smart paradigms

-- Internal format that works for all sources
data AtomWithArity = AA String Int deriving (Show, Eq, Ord)

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
concreteLexicon gname userlexicon poses = let lName = pretty $ lexName gname in
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
    [ "abstract" <+> pretty (lexName gname) <+> "=" <+> "Atoms ** {",
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
    t:_ -> hsep $ map pretty [name pr, "=", hackyRemoveFullPred $ showExpr [] $ snd t, ";"]

-- TODO: handle this function as Gf trees to other Gf trees, not string processing
hackyRemoveFullPred :: String -> String
hackyRemoveFullPred str = case words $ hackyChangeIntToCard $ trim str of
                       "PredAP":_pol:ws -> printf "p1 (ComplAP %s)" $ unwords ws
                       "PredNP":_pol:ws -> printf "p1 (ComplNP %s)" $ unwords ws
                       "p0":ws -> printf "mkAtom %s" $ unwords ws
                      --  "V2PartAdv":_pol:v2:adv
                      --    -> printf "p1 (ComplAP (AdvAP (PastPartAP (mkVPSlash %s)) %s))" v2 (unwords adv)
                       _ -> str

hackyChangeIntToCard :: String -> String
hackyChangeIntToCard str = case splitOn "(Int2Card 1)" str of
                        [] -> str
                        xs -> intercalate "(mkCard \"1\")" xs



--- TODO: filter out predicates based on arity