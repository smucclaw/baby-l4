{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.GenerateLexicon where

import qualified Data.Set as S
import qualified GF
import PGF (PGF, readPGF)
import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)
import ToGF.FromSCasp.SCasp as SC hiding (parens)
import System.Environment (withArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)
import Data.List.Extra (splitOn)

----------------------------------------------------------------------
-- Generate GF code

grName, topName, lexName :: Doc () --String
grName = "Answer"
topName = grName <> "Top"
lexName = grName <> "Lexicon"

mkAbsName, mkCncName, mkPGFName :: Doc () -> String
mkAbsName d = printf "grammars/%s.gf" (show d)
mkCncName d = printf "grammars/%sEng.gf" (show d)
mkPGFName d = printf "/tmp/%s.pgf" (show d)

createPGF :: IO PGF.PGF
createPGF = do
  withArgs
    [ "-make",
      "--output-dir=/tmp",
      "--gfo-dir=/tmp",
      "-v=0",
      mkCncName topName
    ]
    GF.main
  PGF.readPGF $ mkPGFName topName

createGF :: Model -> IO ()
createGF model = do
  let (absS, cncS) = mkLexicon model
  writeDoc (mkAbsName lexName) absS
  writeDoc (mkCncName lexName) cncS
  writeDoc (mkAbsName topName) $ "abstract " <> topName <+> "=" <+> grName <> "," <+> lexName <+> "** {flags startcat = Statement ;}"
  writeDoc (mkCncName topName) $ "concrete " <> topName <> "Eng of " <> topName <+> "=" <+> grName <> "Eng," <+> lexName <> "Eng ;"

writeDoc :: FilePath -> Doc () -> IO ()
writeDoc name doc = withFile name WriteMode $ \h -> hPutDoc h doc

mkLexicon :: SC.Tree s -> (Doc (), Doc ())
mkLexicon model = (abstractLexicon lexicon, concreteLexicon lexicon)
  where
    lexicon = guessPOS <$> S.toList (getAtoms model)

concreteLexicon :: [POS] -> Doc ()
concreteLexicon poses =
  vsep
    [ "concrete" <+> lexName <> "Eng of" <+> lexName <+> "=" <+> grName <> "Eng ** open SyntaxEng, ParadigmsEng in {",
      "lin",
      (indent 4 . vsep) (concrEntry <$> poses),
      "}"
    ]

abstractLexicon :: [POS] -> Doc ()
abstractLexicon poses =
  vsep
    [ "abstract" <+> lexName <+> "=" <+> grName <+> "** {",
      "fun",
      indent 4 . sep . punctuate "," . map (pretty . origName) $ poses,
      indent 4 ": Atom ;",
      "}"
    ]

concrEntry :: POS -> Doc ()
concrEntry (POS name p) = hsep [pretty name, "=", "mkAtom", parens $ innerLex p, ";"]
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

type Prep = Maybe String

data POS = POS {origName :: String, pos :: InnerPOS}
          deriving (Eq, Show)

data InnerPOS = PN2 String Prep | PN String | PV2 String Prep | PV String
          deriving (Eq, Show)

guessPOS :: AtomWithArity -> POS
guessPOS aa@(AA str int) = POS str $ case (int, splitOn "_" str) of
  (0, [noun]) -> PN noun
  (2, ["is", noun, prep]) -> PN2 noun (Just prep) -- e.g. is_participant_in
  (2, ["is", noun]) -> PN2 noun Nothing           -- e.g. is_winner
  (1, ["is", noun]) -> PN noun                    -- e.g. is_game
  (1, ["is", noun, _]) -> PN noun                 -- for completeness' sake
  (1, [verb]) -> PV verb
  (2, [verb]) -> PV2 verb Nothing
  (2, [verb, prep]) -> PV2 verb (Just prep)
  _ -> error $ "guessPOS: unexpected output " ++ show aa

