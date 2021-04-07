{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module ToGF.FromL4.ToProp where

-- the generated Haskell abstract syntax from the GF

import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import qualified GF
import PGF
import Prop
import Syntax
import System.Environment (withArgs)
import System.IO (stderr, hPutStrLn)
import Text.Printf (printf)
import ToGF.FromL4.TransProp
import ToGF.NormalizeSyntax ( varName )

-- moved this here from exe/Main.hs, needed to tell optparse which languages to output
data GFlang  = GFall | GFeng | GFswe deriving Show

gfl2lang:: GFlang -> [Lang]
gfl2lang gfLang =
  case gfLang of
    GFall -> ["Eng","Swe"]
    GFeng -> ["Eng"]
    GFswe -> ["Swe"]

createPGF :: (Show et) => GFlang -> Program et -> IO PGF.PGF
createPGF gfl (Program _ lexicon _2 _3 _4 _5) = do
  let langs = gfl2lang gfl
  let (abstract,concretes) = createLexicon langs lexicon
  -- Generate lexicon
  writeFile "grammars/PropLexicon.gf" abstract
  forM_ (zip langs concretes) $
    \(lang, concrete) -> writeFile (concrName "PropLexicon" lang) concrete
  -- Generate Top module
  let topAbs = "abstract PropTop = Prop, PropLexicon ;"
  let topCnc lang = printf "concrete PropTop%s of PropTop = Prop%s, PropLexicon%s ;" lang lang lang
  writeFile "grammars/PropTop.gf" topAbs
  forM_ langs $
    \lang -> writeFile (concrName "PropTop" lang) (topCnc lang)
  withArgs (["-make", "--output-dir=generated", "-v=0"] ++ map (concrName "PropTop") langs) GF.main
  PGF.readPGF "generated/PropTop.pgf"

nlg, nlgAST :: GFlang -> Program Tp -> IO ()
nlg = nlg' False
nlgAST = nlg' True

nlg' :: Bool -> GFlang -> Program Tp -> IO ()
nlg' showAST gfl prog = do
    gr <- createPGF gfl prog
    sequence_
      [ do
          if showAST
            then hPutStrLn stderr $ "\nGF AST\n" ++ PGF.showExpr [] pgfExpr
            else putStrLn "\n"
          -- putStrLn "DIRECT TRANSLATION"
          -- mapM_ putStrLn $ linearizeAll gr pgfExpr
          -- putStrLn "MORE NATURAL"
          mapM_ putStrLn $ linearizeAll gr optpgf
        | prop <- program2prop prog,
          let pgfExpr = gf prop,
          let optpgf = transfer MOptimize pgfExpr
      ]

-----------------------------------------------------------------------------

type CuteCats = Reader Env

data Env
  = Env
      { lexicon :: [Mapping Tp],
        vardecls :: [[VarDecl Tp]]
      }

program2prop :: Program Tp -> [GProp]
program2prop e = case e of
  Program _ lex _cl vars rules _as ->
    let env0 = Env {lexicon = lex, vardecls = [vars]}
     in runReader
          (mapM rule2prop rules)
          env0

vardecl2prop :: VarDecl t -> CuteCats GProp
vardecl2prop (VarDecl _ vname vtyp) = do
  typ <- tp2kind vtyp
  name <- var2ind (GlobalVar vname)
  pure $ GPAtom (GAKind typ name)

var2ind :: Var -> CuteCats GInd
var2ind var = do
  let name = varName var
  lex <- asks lexicon
  return $ case findMapping lex name of
    val : _ | gfType val == "Noun" -> GIVarN (LexNoun name)
    _ -> GIVar (GVString (GString name)) -- Fall back to string literal

var2pred :: Var -> CuteCats GPred1
var2pred var = do
  let name = varName var
  lex <- asks lexicon
  return $ case findMapping lex name of
    val : _
      | gfType val == "Adj" -> GPAdj1 (LexAdj name)
      | gfType val == "Verb" -> GPVerb1 (LexVerb name)
      | gfType val == "Noun" -> GPNoun1 (LexNoun name)
    _ -> GPVar1 (GVString (GString name))

var2pred2 :: Var -> CuteCats GPred2
var2pred2 var = do
  let name = varName var
  lex <- asks lexicon
  return $ case findMapping lex name of
    val : _ | gfType val == "Adj2" -> GPAdj2 (LexAdj2 name)
    val : _ | gfType val == "Verb2" -> GPVerb2 (LexVerb2 name)
    val : _ | gfType val == "Noun2" -> GPNoun2 (LexNoun2 name)
    _ -> GPVar2 (GVString (GString name))

tp2kind :: Tp -> CuteCats GKind
tp2kind e = case e of
  BoolT -> pure GBoolean
  IntT -> pure GNat
  ClassT (ClsNm name) -> pure $ GKNoun (LexNoun name)
  FunT arg ret -> GKFun <$> tp2kind arg <*> tp2kind ret
  -- TupleT [Tp]
  -- ErrT
  _ -> error $ "tp2kind: not yet supported: " ++ show e

tp2ind :: Tp -> CuteCats GInd
tp2ind e = case e of
  --BoolT -> pure GBoolean
  --IntT -> pure GNat
  ClassT (ClsNm name) -> pure $ GINoun (LexNoun name)
  --FunT arg ret -> GKFun <$> tp2kind arg <*> tp2kind ret
  -- TupleT [Tp]
  -- ErrT
  _ -> pure $ GIVar (GVString (GString "<unsupported>"))
  -- _ -> error $ "tp2kind: not yet supported: " ++ show e


rule2prop :: Rule Tp -> CuteCats GProp
rule2prop (Rule _ nm vars ifE thenE) = local (updateVars vars) $
  do
    ifProp <- expr2prop ifE
    thenProp <- expr2prop thenE
    return $ GPImpl ifProp thenProp

expr2prop :: Syntax.Expr Tp -> CuteCats GProp
expr2prop e = case e of
  ValE _ val -> pure $ GPAtom (val2atom val)
  FunApp1 f x xTp -> do
    f' <- var2pred f
--    x' <- tp2ind xTp
    x' <- var2ind x
    pure $ GPAtom (GAPred1 f' x')
  FunApp2 f x xTp y yTp -> do
    f' <- var2pred2 f
    x' <- tp2ind xTp
    y' <- tp2ind yTp
    pure $ GPAtom (GAPred2 f' x' y')
  Exist x cl exp -> do
    prop <- expr2prop exp
    typ <- tp2kind cl
    pure $ GPExists (GListVar [GVString (GString x)]) typ prop
  Forall x cl exp -> do
    prop <- expr2prop exp
    typ <- tp2kind cl
    pure $ GPUnivs (GListVar [GVString (GString x)]) typ prop
  And e1 e2 -> do
    exp1 <- expr2prop e1
    exp2 <- expr2prop e2
    pure $ GPConj GCAnd exp1 exp2
  Or e1 e2 -> do
    exp1 <- expr2prop e1
    exp2 <- expr2prop e2
    pure $ GPConj GCOr exp1 exp2
  Impl e1 e2 -> do
    exp1 <- expr2prop e1
    exp2 <- expr2prop e2
    pure $ GPImpl exp1 exp2
  Not e -> GPNeg <$> expr2prop e
  TupleE _ es -> do
    props <- mapM expr2prop es
    pure $ GPConjs GCAnd (GListProp props)
  IfThenElse e1 e2 e3 -> do
    exp1 <- expr2prop e1
    exp2 <- expr2prop e2
    exp3 <- expr2prop e3
    pure $ GPIfThenElse exp1 exp2 exp3
  --VarE _ var -> var2prop var
  _ -> error $ "expr2prop: not yet supported: " ++ show e

val2atom :: Val -> GAtom
val2atom e = case e of
  BoolV True -> GAKind GBoolean GBTrue
  BoolV False -> GAKind GBoolean GBFalse
  IntV i -> GAKind GNat (GIInt (GInt (fromInteger i)))
  _ -> error $ "val2atom: not yet supported: " ++ show e

----------------------------------------
-- Patterns

pattern AppU :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern AppU x y <- AppE _ x y

pattern VarU :: Var -> Tp -> Syntax.Expr Tp
pattern VarU var tp <- VarE tp var

pattern FunApp1 :: Var -> Var -> Tp -> Syntax.Expr Tp
pattern FunApp1 f x xTp <- AppU (VarU f _) (VarU x xTp)

-- AppU (VarU (GlobalVar f)) (VarU (LocalVar x int))

pattern FunApp2 :: Var -> Var -> Tp -> Var -> Tp -> Syntax.Expr Tp
pattern FunApp2 f x xTp y yTp <- AppU (FunApp1 f x xTp) (VarU y yTp)

-- Quantification

pattern Exist :: VarName -> Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Exist x typ exp <- QuantifE _ Ex x typ exp

pattern Forall :: VarName -> Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Forall x typ exp <- QuantifE _ All x typ exp

-- Binary operations

pattern And :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern And e1 e2 <- BinOpE _ (BBool BBand) e1 e2

pattern Or :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Or e1 e2 <- BinOpE _ (BBool BBor) e1 e2

pattern Not :: Syntax.Expr Tp -> Syntax.Expr Tp
pattern Not e <- UnaOpE _ (UBool UBneg) e

pattern Impl :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Impl e1 e2 <- BinOpE _ (BBool BBimpl) e1 e2

pattern IfThenElse :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern IfThenElse e1 e2 e3 <- IfThenElseE _ e1 e2 e3

----------------------------------------
-- Generic helper functions

updateVars :: [VarDecl Tp] -> Env -> Env
updateVars vs env = env {vardecls = vs : vardecls env}

findMapping :: [Mapping t] -> String -> [String]
findMapping haystack needle =
  [ val
    | Mapping _ name val <- haystack,
      name == needle
  ]

type Lang = String

createLexicon :: [Lang] -> [Mapping t] -> (String, [String])
createLexicon langs lexicon = (abstract, concretes)
  where
    abstract =
      unlines $
        ["abstract PropLexicon = Prop ** {"]
          ++ ["fun"]
          ++ [ printf "%s : %s ;" name (gfType val)
               | Mapping _ name val <- lexicon
             ]
          ++ ["}"]
    concretes =
      [ unlines $
          [printf "concrete PropLexicon%s of PropLexicon = Prop%s ** open WordNet%s, Paradigms%s, Syntax%s, Extend%s in {" lang lang lang lang lang lang]
            ++ ["lin"]
            ++ [ printf "%s = %s ;" name val
                 | Mapping _ name val <- lexicon
               ]
            ++ [printf "oper associated_A = mkA \"%s\" ;" associated]
            ++ ["}"]
        | lang <- langs,
          let associated = if lang == "Swe" then "associerad" else "associated"
      ]

concrName :: String -> Lang -> String
concrName = printf "grammars/%s%s.gf"

gfType :: String -> String
gfType str = case (reverse . takeWhile (/= '_') . reverse) str of
  "N" -> "Noun"
  "A" -> "Adj"
  "V" -> "Verb"
  "N2" -> "Noun2"
  "A2" -> "Adj2"
  "V2" -> "Verb2"
  _ -> case take 4 str of
    "mkA2" -> "Adj2"
    "mkV2" -> "Verb2"
    "mkN2" -> "Noun2"
    "mkA " -> "Adj"
    "mkV " -> "Verb"
    "mkN " -> "Noun"
    "Comp" -> "Noun" -- hack: to support CompoundN
    _ -> error $ "gfType: not supported yet: " ++ str
