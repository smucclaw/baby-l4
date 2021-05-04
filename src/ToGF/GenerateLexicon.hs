{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.GenerateLexicon where

import qualified Data.Set as S
import qualified GF
import PGF (PGF, Expr, readPGF, linearizeAll)
import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)
import ToGF.FromSCasp.SCasp as SC hiding (parens)
import System.Environment (withArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)
import Data.List.Extra (splitOn)
import Data.Set (Set)

----------------------------------------------------------------------
-- Generate GF code

type GrName = String

topName, lexName :: GrName -> Doc () --String
topName grName = pretty grName <> "Top"
lexName grName = pretty grName <> "Lexicon"

mkAbsName, mkCncName, mkPGFName :: Doc () -> String
mkAbsName d = printf "grammars/%s.gf" (show d)
mkCncName d = printf "grammars/%sEng.gf" (show d)
mkPGFName d = printf "generated/%s.pgf" (show d)

createPGF :: Doc () -> IO PGF.PGF
createPGF name = do
  withArgs
    [ "-make",
      "--output-dir=generated",
      "--gfo-dir=generated",
      "-v=0",
      mkCncName name
    ]
    GF.main
  PGF.readPGF $ mkPGFName name

createGF' :: GrName -> [AtomWithArity] -> IO PGF.PGF
createGF' gname model = do
  let lName = lexName gname
      tName = topName gname
      grName = pretty gname
  let (absS, cncS) = mkLexicon gname model
  writeDoc (mkAbsName lName) absS
  writeDoc (mkCncName lName) cncS
  writeDoc (mkAbsName tName) $ "abstract" <+> tName <+> "=" <+> grName <> "," <+> lName <+> "** {flags startcat = Statement ;}"
  writeDoc (mkCncName tName) $ "concrete" <+> tName <> "Eng of " <> tName <+> "=" <+> grName <> "Eng," <+> lName <> "Eng ;"
  createPGF tName

writeDoc :: FilePath -> Doc () -> IO ()
writeDoc name doc = withFile name WriteMode $ \h -> hPutDoc h doc

mkLexicon :: GrName -> [AtomWithArity] -> (Doc (), Doc ())
mkLexicon gname atoms = (abstractLexicon gname lexicon, concreteLexicon gname lexicon)
  where
    lexicon = guessPOS <$> atoms

printGF' :: PGF -> Expr -> IO ()
printGF' gr expr = do
  --putStrLn $ showExpr [] $ gf expr
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

data POS = POS {origName :: String, pos :: InnerPOS}

data InnerPOS = PN2 String Prep | PN String | PV2 String Prep | PV String

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


concreteLexicon :: GrName -> [POS] -> Doc ()
concreteLexicon gname poses = let lName = lexName gname in
  vsep
    [ "concrete" <+> lName <> "Eng of" <+> lName <+> "=" <+> pretty gname <> "Eng ** open SyntaxEng, ParadigmsEng in {",
      "lin",
      (indent 4 . vsep) (concrEntry <$> poses),
      "}"
    ]

abstractLexicon :: GrName -> [POS] -> Doc ()
abstractLexicon gname poses =
  vsep
    [ "abstract" <+> lexName gname <+> "=" <+> pretty gname <+> "** {",
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


