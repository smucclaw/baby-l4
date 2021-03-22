module ToGF where

import Syntax
import Prop -- the generated Haskell abstract syntax from the GF
import PGF
import Paths_baby_l4
import qualified GF
import System.Environment (withArgs)
import System.IO (stderr, hPutStrLn)
import Control.Monad (forM_)
import Text.Printf (printf)

-- moved this here from exe/Main.hs, needed to tell optparse which languages to output
data GFlang  = GFall | GFeng | GFswe deriving Show

gfl2lang:: GFlang -> [Lang]
gfl2lang gfLang =
  case gfLang of
    GFall -> ["Eng","Swe"]
    GFeng -> ["Eng"]
    GFswe -> ["Swe"]

createPGF :: (Show ct, Show et) => GFlang -> Program ct et -> IO PGF.PGF
createPGF gfl (Program lexicon _2 _3 _4 _5) = do
  let langs = gfl2lang gfl
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

  withArgs (["-make", "--output-dir=generated", "-v=0"] ++ map (concrName "PropTop") langs) GF.main
  PGF.readPGF "generated/PropTop.pgf"

nlgAST :: (Show ct, Show et) => GFlang -> Program ct et -> IO ()
nlgAST gfl prog = do
  gr <- createPGF gfl prog
  sequence_ [ do
    hPutStrLn stderr $ PGF.showExpr [] pgfExpr
    mapM_ putStrLn $ linearizeAll gr pgfExpr
    | prop <- program2prop prog
    , let pgfExpr = gf prop
    ]

nlg :: (Show ct, Show et) => GFlang -> Program ct et -> IO ()
nlg gfl prog = do
  gr <- createPGF gfl prog
  sequence_ [ do
    mapM_ putStrLn $ linearizeAll gr pgfExpr
    | prop <- program2prop prog
    , let pgfExpr = gf prop
    ]

program2prop :: (Show ct, Show et) => Program ct et -> [GProp]
program2prop e = case e of
  Program lexicon _cl vardecls _rs _as -> map (vardecl2prop lexicon) vardecls
 -- _ -> error $ "program2prop: not yet supported: " ++ show e

vardecl2prop :: [Mapping] -> VarDecl -> GProp
vardecl2prop lex (VarDecl vname vtyp) =
  GPAtom (GAKind (typ2kind lex vtyp) (var2ind lex vname))

var2ind :: [Mapping] -> VarName -> GInd
var2ind lexicon name = case findMapping lexicon name of
    val:_ -> if gfType val == "Noun"
              then GIVarN (LexNoun name)
              else GIVar (GVString (GString name))
    _           -> GIVar (GVString (GString name)) -- Fall back to string literal

typ2kind :: [Mapping] -> Tp -> GKind
typ2kind lexicon e = case e of
  BoolT -> GBoolean
  IntT  -> GNat
  ClassT (ClsNm name) -> GKInd (var2ind lexicon name)
  FunT arg ret -> GKFun (typ2kind lexicon arg) (typ2kind lexicon ret)
  _ -> error $ "typ2kind: not yet supported: " ++ show e
  -- TupleT [Tp]
  -- ErrT

expr2prop :: Syntax.Expr Tp -> GProp
expr2prop e = case e of
  ValE _ _ val -> GPAtom (val2atom val)
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
  | Mapping _ name val <- haystack
  , name == needle ]

type Lang = String

createLexicon :: [Lang] -> [Mapping] -> (String,[String])
createLexicon langs lexicon = (abstract,concretes)
  where
    abstract = unlines $
      ["abstract PropLexicon = Prop ** {"] ++
      ["fun"] ++
      [ printf "%s : %s ;" name (gfType val)
      | Mapping _ name val <- lexicon ] ++
      ["}"]
    concretes =
      [ unlines $
          [printf "concrete PropLexicon%s of PropLexicon = Prop%s ** open WordNet%s in {" lang lang lang] ++
          ["lin"] ++
          [printf "%s = %s ;" name val
          | Mapping _ name val <- lexicon ] ++
          ["}"]
      | lang <- langs ]

concrName :: String -> Lang -> String
concrName = printf "grammars/%s%s.gf"

gfType :: String -> String
gfType str = case last str of
  'N' -> "Noun"
  'A' -> "Adj"
  'V' -> "Verb"
  _ -> error $ "gfType: not supported yet: " ++ str