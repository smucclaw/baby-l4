module ToGF where

import Syntax
import Prop -- the generated Haskell abstract syntax from the GF
import PGF
import Paths_baby_l4
import qualified GF
import System.Environment (withArgs)
import Control.Monad (forM_)
import Text.Printf (printf)


-- myPgf :: IO PGF
-- myPgf = do
--   -- pgfFile <- Paths_baby_l4.getDataFileName "Prop.pgf"
--   -- TODO: create the PGF file!
--   let pgfFile = "generated/PropTop.pgf"
--   PGF.readPGF pgfFile

createPGF :: (Show ct, Show et) => Program ct et -> IO PGF.PGF
createPGF (Program lexicon _2 _3 _4 _5) = do
  let langs = ["Eng","Swe"]
  let (abstract,concretes) = createLexicon langs lexicon
  -- Generate lexicon
  writeFile "grammars/PropLexicon.gf" abstract
  forM_ (zip langs concretes) $
    \(lang,concrete) -> writeFile (concrName "PropLexicon" lang) concrete

  -- Generate Top module
  let topAbs = "abstract PropTop = Prop, PropLexicon ;"
  let topCnc lang = printf "concrete PropTop%s of PropTop = Prop%s, PropLexicon%s ;" lang lang lang

  writeFile "grammars/PropTop.gf" topAbs
  forM_ langs $
    \lang -> writeFile (concrName "PropTop" lang) (topCnc lang)

  withArgs ["-make", "--output-dir=generated", "grammars/PropTopEng.gf", "grammars/PropTopSwe.gf"] GF.main
  PGF.readPGF "generated/PropTop.pgf"


-- printPgf :: IO ()
-- printPgf = print . categories =<< myPgf

nlg :: (Show ct, Show et) => Program ct et -> IO ()
nlg prog = do
  gr <- createPGF prog
  sequence_ [
    mapM_ putStrLn $ linearizeAll gr pgfExpr
    | prop <- program2prop prog
    , let pgfExpr = gf prop
    ]

-- Example: Program [] [VarDecl "myNumber" IntT,VarDecl "myTruth" BoolT] [] []
program2prop :: (Show ct, Show et) => Program ct et -> [GProp]
program2prop e = case e of
  Program lexicon _cl vardecls _rs _as -> map (vardecl2prop lexicon) vardecls
 -- _ -> error $ "program2prop: not yet supported: " ++ show e

vardecl2prop :: [Mapping] -> VarDecl -> GProp
vardecl2prop lex (VarDecl vname vtyp) =
  GPAtom (GAKind (typ2kind vtyp) (var2ind lex vname))

var2ind :: [Mapping] -> VarName -> GInd
var2ind lexicon name = case findMapping lexicon name of
    val:_ -> GIVarN (LexNoun name)
    []    -> GIVar (GVString (GString name)) -- Fall back to string literal

typ2kind :: Tp -> GKind
typ2kind e = case e of
  BoolT -> GBoolean
  IntT  -> GNat
  _ -> error $ "typ2kind: not yet supported: " ++ show e
  -- ClassT ClassName
  -- FunT Tp Tp
  -- TupleT [Tp]
  -- ErrT

expr2prop :: Syntax.Expr Tp -> GProp
expr2prop e = case e of
  ValE _ val -> GPAtom (val2atom val)
  _ -> error $ "expr2prop: not yet supported: " ++ show e

val2atom :: Val -> GAtom
val2atom e  = case e of
  BoolV True -> GAKind GBoolean GBTrue
  BoolV False -> GAKind GBoolean GBFalse
  IntV i -> GAKind GNat (GIInt (GInt (fromInteger i)))
  _ -> error $ "val2atom: not yet supported: " ++ show e

-- Generic helper functions
findMapping :: [Mapping] -> String -> [String]
findMapping haystack needle =
  [ val
  | Mapping name val <- haystack
  , name == needle ]

type Lang = String

createLexicon :: [Lang] -> [Mapping] -> (String,[String])
createLexicon langs lexicon = (abstract,concretes)
  where
    abstract = unlines $
      ["abstract PropLexicon = Prop ** {"] ++
      ["fun"] ++
      [ name ++ " : " ++ "Noun ;"| Mapping name _ <- lexicon ] ++
      ["}"]
    concretes =
      [ unlines $
          [printf "concrete PropLexicon%s of PropLexicon = Prop%s ** open WordNet%s in {" lang lang lang] ++
          ["lin"] ++
          [printf "%s = %s ;" name val
          | Mapping name val <- lexicon ] ++
          ["}"]
      | lang <- langs ]

concrName :: String -> Lang -> String
concrName = printf "grammars/%s%s.gf"