{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
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
import TransProp

createPGF :: (Show ct, Show et) => Program ct et -> IO PGF.PGF
createPGF (Program lexicon _2 _3 _4 _5) = do
  let langs = ["Eng"]
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

nlg :: (Show ct) => Program ct Tp -> IO ()
nlg prog = do
  gr <- createPGF prog
  sequence_
    [ do
        -- putStrLn $ PGF.showExpr [] pgfExpr
        putStrLn ""
        putStrLn "direct translation from logic"
        mapM_ putStrLn $ linearizeAll gr pgfExpr
        putStrLn "more natural"
        mapM_ putStrLn $ linearizeAll gr optpgf
      | prop <- program2prop prog,
        let pgfExpr = gf prop,
        let optpgf = transfer MOptimize pgfExpr
    ]

-----------------------------------------------------------------------------

type CuteCats = Reader Env

data Env
  = Env
      { lexicon :: [Mapping],
        vardecls :: [[VarDecl]]
      }

program2prop :: (Show ct) => Program ct Tp -> [GProp]
program2prop e = case e of
  Program lex _cl vars rules _as ->
    let env0 = Env {lexicon = lex, vardecls = [vars]}
     in runReader
          (mapM rule2prop rules)
          env0

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

typ2kind :: Tp -> CuteCats GKind
typ2kind e = case e of
  BoolT -> pure GBoolean
  IntT -> pure GNat
  ClassT (ClsNm name) -> pure $ GKNoun (LexNoun name)
  FunT arg ret -> GKFun <$> typ2kind arg <*> typ2kind ret
  -- TupleT [Tp]
  -- ErrT
  _ -> error $ "typ2kind: not yet supported: " ++ show e

rule2prop :: Rule Tp -> CuteCats GProp
rule2prop (Rule nm vars ifE thenE) = local (updateVars vars) $
  do
    ifProp <- expr2prop ifE
    thenProp <- expr2prop thenE
    return $ GPImpl ifProp thenProp

expr2prop :: Syntax.Expr Tp -> CuteCats GProp
expr2prop e = case e of
  ValE _ _ val -> pure $ GPAtom (val2atom val)
  FunApp1 f x xTp -> do
    f' <- var2pred f
    x' <- var2ind x
    pure $ GPAtom (GAPred1 f' x')
  FunApp2 f x xTp y yTp -> do
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
  TupleE _ _ es -> do
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
pattern AppU x y <- AppE _ _ x y

pattern VarU :: Var -> Tp -> Syntax.Expr Tp
pattern VarU var tp <- VarE _ tp var

pattern FunApp1 :: Var -> Var -> Tp -> Syntax.Expr Tp
pattern FunApp1 f x xTp <- AppU (VarU f _) (VarU x xTp)

-- AppU (VarU (GlobalVar f)) (VarU (LocalVar x int))

pattern FunApp2 :: Var -> Var -> Tp -> Var -> Tp -> Syntax.Expr Tp
pattern FunApp2 f x xTp y yTp <- AppU (FunApp1 f x xTp) (VarU y yTp)

-- Quantification

pattern Exist :: VarName -> Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Exist x typ exp <- QuantifE _ _ Ex x typ exp

pattern Forall :: VarName -> Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Forall x typ exp <- QuantifE _ _ All x typ exp

-- Binary operations

pattern And :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern And e1 e2 <- BinOpE _ _ (BBool BBand) e1 e2

pattern Or :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Or e1 e2 <- BinOpE _ _ (BBool BBor) e1 e2

pattern Not :: Syntax.Expr Tp -> Syntax.Expr Tp
pattern Not e <- UnaOpE _ _ (UBool UBneg) e

pattern Impl :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern Impl e1 e2 <- BinOpE _ _ (BBool BBimpl) e1 e2

pattern IfThenElse :: Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp -> Syntax.Expr Tp
pattern IfThenElse e1 e2 e3 <- IfThenElseE _ _ e1 e2 e3

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
    | Mapping _ name val <- haystack,
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
               | Mapping _ name val <- lexicon
             ]
          ++ ["}"]
    concretes =
      [ unlines $
          [printf "concrete PropLexicon%s of PropLexicon = Prop%s ** open WordNet%s, Paradigms%s, Extend%s in {" lang lang lang lang lang]
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

foo =
  Program
    [Mapping (SRng {start = Pos {line = 3, col = 0}, end = Pos {line = 3, col = 26}}) "Business" "business_1_N", Mapping (SRng {start = Pos {line = 4, col = 0}, end = Pos {line = 4, col = 51}}) "BusinessEntity" "CompoundN business_1_N entity_N", Mapping (SRng {start = Pos {line = 5, col = 0}, end = Pos {line = 5, col = 56}}) "IncompatibleDignity" "mkA2 incompatible_1_A with_Prep", Mapping (SRng {start = Pos {line = 6, col = 0}, end = Pos {line = 6, col = 31}}) "LegalPractitioner" "lawyer_N", Mapping (SRng {start = Pos {line = 7, col = 0}, end = Pos {line = 7, col = 28}}) "LocumSolicitor" "lawyer_N", Mapping (SRng {start = Pos {line = 8, col = 0}, end = Pos {line = 8, col = 26}}) "AcceptApp" "accept_4_V2", Mapping (SRng {start = Pos {line = 9, col = 0}, end = Pos {line = 9, col = 47}}) "AssociatedWith" "mkA2 associated_A with_Prep", Mapping (SRng {start = Pos {line = 10, col = 0}, end = Pos {line = 10, col = 32}}) "Appointment" "appointment_1_N", Mapping (SRng {start = Pos {line = 11, col = 0}, end = Pos {line = 11, col = 34}}) "LawRelatedService" "service_1_N", Mapping (SRng {start = Pos {line = 12, col = 0}, end = Pos {line = 12, col = 26}}) "Provides" "provide_1_V2", Mapping (SRng {start = Pos {line = 13, col = 0}, end = Pos {line = 13, col = 44}}) "ConditionsSecondSchedule" "condition_3_V2", Mapping (SRng {start = Pos {line = 14, col = 0}, end = Pos {line = 14, col = 21}}) "App" "appoint_2_V2"]
    []
    []
    [ Rule
        "r1a"
        [VarDecl "lpr" (ClassT (ClsNm "LegalPractitioner")), VarDecl "app" (ClassT (ClsNm "Appointment"))]
        ( UnaOpE
            (SRng {start = Pos {line = 18, col = 3}, end = Pos {line = 18, col = 53}})
            ErrT
            (UBool UBneg)
            ( QuantifE
                (SRng {start = Pos {line = 18, col = 8}, end = Pos {line = 18, col = 53}})
                ErrT
                Ex
                "bsn"
                (ClassT (ClsNm "Business"))
                ( AppE
                    (SRng {start = Pos {line = 18, col = 31}, end = Pos {line = 18, col = 53}})
                    ErrT
                    ( AppE
                        (SRng {start = Pos {line = 18, col = 31}, end = Pos {line = 18, col = 49}})
                        ErrT
                        ( VarE
                            ( SRng
                                { start = Pos {line = 18, col = 31},
                                  end = Pos {line = 18, col = 45}
                                }
                            )
                            ErrT
                            (GlobalVar "AssociatedWith")
                        )
                        -- x
                        ( VarE
                            (SRng {start = Pos {line = 18, col = 46}, end = Pos {line = 18, col = 49}})
                            (ClassT (ClsNm "Business"))
                            (LocalVar "bsn" 0)
                        )
                    )
                    -- y
                    ( VarE
                        (SRng {start = Pos {line = 18, col = 50}, end = Pos {line = 18, col = 53}})
                        (ClassT (ClsNm "Appointment"))
                        (LocalVar "app" 1)
                    )
                )
            )
        )
        ( UnaOpE
            (SRng {start = Pos {line = 19, col = 5}, end = Pos {line = 19, col = 27}})
            ErrT
            (UBool UBneg)
            ( AppE
                (SRng {start = Pos {line = 19, col = 10}, end = Pos {line = 19, col = 27}})
                ErrT
                ( AppE
                    (SRng {start = Pos {line = 19, col = 10}, end = Pos {line = 19, col = 23}})
                    ErrT
                    (VarE (SRng {start = Pos {line = 19, col = 10}, end = Pos {line = 19, col = 19}}) ErrT (GlobalVar "AcceptApp"))
                    ( VarE
                        (SRng {start = Pos {line = 19, col = 20}, end = Pos {line = 19, col = 23}})
                        (ClassT (ClsNm "LegalPractitioner"))
                        (LocalVar "lpr" 1)
                    )
                )
                (VarE (SRng {start = Pos {line = 19, col = 24}, end = Pos {line = 19, col = 27}}) (ClassT (ClsNm "Appointment")) (LocalVar "app" 0))
            )
        )
    ]
    []
