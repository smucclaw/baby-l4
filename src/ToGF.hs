{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ToGF where

-- the generated Haskell abstract syntax from the GF

import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import qualified GF
import PGF
import Prop
import Syntax
import System.Environment (withArgs)
import Text.Printf (printf)

createPGF :: (Show ct) => Program ct () -> IO PGF.PGF
createPGF (Program lexicon _2 _3 _4 _5) = do
  let langs = ["Eng", "Swe"]
  let (abstract, concretes) = createLexicon langs lexicon
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
  withArgs (["-make", "--output-dir=generated", "--gfo-dir=/tmp", "-v=0"] ++ map (concrName "PropTop") langs) GF.main
  PGF.readPGF "generated/PropTop.pgf"

nlg :: (Show ct) => Program ct () -> IO ()
nlg prog = do
  gr <- createPGF prog
  sequence_
    [ do
        putStrLn $ PGF.showExpr [] pgfExpr
        mapM_ putStrLn $ linearizeAll gr pgfExpr
      | prop <- program2prop prog,
        let pgfExpr = gf prop
    ]

-----------------------------------------------------------------------------

type CuteCats = Reader Env

data Env
  = Env
      { lexicon :: [Mapping],
        vardecls :: [[VarDecl]]
      }

program2prop :: (Show ct) => Program ct () -> [GProp]
program2prop e = case e of
  Program lex _cl vars rules _as ->
    let env0 = Env {lexicon = lex, vardecls = [vars]}
     in runReader
          (mapM rule2prop rules)
          env0

-- _ -> error $ "program2prop: not yet supported: " ++ show e

vardecl2prop :: VarDecl -> CuteCats GProp
vardecl2prop (VarDecl vname vtyp) = do
  typ <- typ2kind vtyp
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
    val : _ | gfType val == "Adj" -> GPAdj1 (LexAdj name)
    val : _ | gfType val == "Verb" -> GPVerb1 (LexVerb name)
    val : _ | gfType val == "Noun" -> GPNoun1 (LexNoun name)
    _ -> error $ "var2pred: not supported yet: " ++ show var

var2pred2 :: Var -> CuteCats GPred2
var2pred2 var = do
  let name = varName var
  lex <- asks lexicon
  return $ case findMapping lex name of
    val : _ | gfType val == "Adj2" -> GPAdj2 (LexAdj2 name)
    val : _ | gfType val == "Verb2" -> GPVerb2 (LexVerb2 name)
    _ -> error $ "var2pred2: not supported yet: " ++ show var

typ2kind :: Tp -> CuteCats GKind
typ2kind e = case e of
  BoolT -> pure GBoolean
  IntT -> pure GNat
  ClassT (ClsNm name) -> pure $ GKNoun (LexNoun name)
  FunT arg ret -> GKFun <$> typ2kind arg <*> typ2kind ret
  -- TupleT [Tp]
  -- ErrT
  _ -> error $ "typ2kind: not yet supported: " ++ show e

rule2prop :: Rule () -> CuteCats GProp
rule2prop (Rule nm vars ifE thenE) = local (updateVars vars) $
  do
    ifProp <- expr2prop ifE
    thenProp <- expr2prop thenE
    return $ GPImpl ifProp thenProp

expr2prop :: Syntax.Expr () -> CuteCats GProp
expr2prop e = case e of
  ValE _ val -> pure $ GPAtom (val2atom val)
  FunApp1 f x -> do
    f' <- var2pred f
    x' <- var2ind x
    pure $ GPAtom (GAPred1 f' x')
  FunApp2 f x y -> do
    f' <- var2pred2 f
    x' <- var2ind x
    y' <- var2ind y
    pure $ GPAtom (GAPred2 f' x' y')
  Exist x cl exp -> do
    prop <- expr2prop exp
    typ <- typ2kind cl
    pure $ GPExists (GListVar [GVString (GString x)]) typ prop
  Forall x cl exp -> do
    prop <- expr2prop exp
    typ <- typ2kind cl
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

pattern AppU :: Syntax.Expr () -> Syntax.Expr () -> Syntax.Expr ()
pattern AppU x y = AppE () x y

pattern VarU :: Var -> Syntax.Expr ()
pattern VarU x = VarE () x

pattern FunApp1 :: Var -> Var -> Syntax.Expr ()
pattern FunApp1 f x = AppU (VarU f) (VarU x)

-- AppU (VarU (GlobalVar f)) (VarU (LocalVar x int))

pattern FunApp2 :: Var -> Var -> Var -> Syntax.Expr ()
pattern FunApp2 f x y = AppU (AppU (VarU f) (VarU x)) (VarU y)

-- Quantification

pattern Exist :: VarName -> Tp -> Syntax.Expr () -> Syntax.Expr ()
pattern Exist x typ exp = QuantifE () Ex x typ exp

pattern Forall :: VarName -> Tp -> Syntax.Expr () -> Syntax.Expr ()
pattern Forall x typ exp = QuantifE () All x typ exp

-- Binary operations

pattern And :: Syntax.Expr () -> Syntax.Expr () -> Syntax.Expr ()
pattern And e1 e2 = BinOpE () (BBool BBand) e1 e2

pattern Or :: Syntax.Expr () -> Syntax.Expr () -> Syntax.Expr ()
pattern Or e1 e2 = BinOpE () (BBool BBor) e1 e2

pattern Not :: Syntax.Expr () -> Syntax.Expr ()
pattern Not e = UnaOpE () (UBool UBneg) e

pattern Impl :: Syntax.Expr () -> Syntax.Expr () -> Syntax.Expr ()
pattern Impl e1 e2 = BinOpE () (BBool BBimpl) e1 e2

----------------------------------------
-- Generic helper functions

varName :: Var -> VarName
varName (GlobalVar n) = n
varName (LocalVar n _) = n

updateVars :: [VarDecl] -> Env -> Env
updateVars vs env = env {vardecls = vs : vardecls env}

findMapping :: [Mapping] -> String -> [String]
findMapping haystack needle =
  [ val
    | Mapping name val <- haystack,
      name == needle
  ]

type Lang = String

createLexicon :: [Lang] -> [Mapping] -> (String, [String])
createLexicon langs lexicon = (abstract, concretes)
  where
    abstract =
      unlines $
        ["abstract PropLexicon = Prop ** {"]
          ++ ["fun"]
          ++ [ printf "%s : %s ;" name (gfType val)
               | Mapping name val <- lexicon
             ]
          ++ ["}"]
    concretes =
      [ unlines $
          [printf "concrete PropLexicon%s of PropLexicon = Prop%s ** open WordNet%s in {" lang lang lang]
            ++ ["lin"]
            ++ [ printf "%s = %s ;" name val
                 | Mapping name val <- lexicon
               ]
            ++ ["}"]
        | lang <- langs
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
  _ -> error $ "gfType: not supported yet: " ++ str

--  foo :: Program Tp
foo =
  Program
    [Mapping "is_legal" "legal_1_A", Mapping "Business" "business_1_N", Mapping "IncompatibleDignity" "incompatible_1_A"]
    []
    [VarDecl "myNumber" IntT, VarDecl "myBusiness" (ClassT (ClsNm "Business")), VarDecl "is_legal" (FunT (ClassT (ClsNm "Business")) BoolT)]
    [ Rule
        "r1a"
        [VarDecl "lpr" (ClassT (ClsNm "LegalPractitioner")), VarDecl "app" (ClassT (ClsNm "Appointment"))]
        ( QuantifE
            ErrT
            Ex
            "bsn"
            (ClassT (ClsNm "Business"))
            ( AppE
                ErrT
                ( AppE
                    ErrT
                    (VarE ErrT (GlobalVar "IncompatibleDignity"))
                    (VarE (ClassT (ClsNm "Business")) (LocalVar "bsn" 0))
                )
                (VarE (ClassT (ClsNm "Appointment")) (LocalVar "app" 1))
            )
        )
        ( AppE
            ErrT
            (AppE ErrT (VarE ErrT (GlobalVar "MustNotAcceptApp")) (VarE (ClassT (ClsNm "LegalPractitioner")) (LocalVar "lpr" 1)))
            ( VarE
                (ClassT (ClsNm "Appointment"))
                (LocalVar "app" 0)
            )
        )
    ]
    []
