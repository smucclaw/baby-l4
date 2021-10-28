{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module ToGF.FromL4.ToProp where

-- the generated Haskell abstract syntax from the GF

import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import qualified Data.Set as S
import PGF ( PGF, showExpr, linearizeAll )
import Prop
import L4.Annotation ( HasDefault(defaultVal) )
import L4.Syntax as Syntax
import System.IO (stderr, hPutStrLn)
import Text.Printf (printf)
import ToGF.FromL4.TransProp ( transfer, Mode(MOptimize) )
import ToGF.NormalizeSyntax ( varName, normalizeQuantifGF, pattern BoolT, pattern IntT )
import ToGF.GenerateLexicon
    ( AtomWithArity(..), GrName, createGF' )
import Debug.Trace (trace)

-- moved this here from exe/Main.hs, needed to tell optparse which languages to output
data GFlang  = GFall | GFeng | GFswe deriving (Show,Eq)

gfl2lang:: GFlang -> [Lang]
gfl2lang gfLang =
  case gfLang of
    GFall -> ["Eng","Swe"]
    GFeng -> ["Eng"]
    GFswe -> ["Swe"]

-- Helper functions from GenerateLexicon specialised for Answers
grName :: GrName
grName = "Prop"

createGF :: FilePath -> Program t -> IO PGF
createGF fname prog = trace ("allPreds: " ++ show allPreds) $ createGF' fname grName (lexiconOfProgram prog) allPreds
  where
    allPreds = S.toList $ S.fromList $ concat
      [ getAtoms vardecl
      | vardecl <- globalsOfProgram prog
      ]

getAtoms :: VarDecl t -> [AtomWithArity]
getAtoms (VarDecl _ name tp) =
    AA name (getArity tp) : [ AA nm 0 | nm <- getNames tp]
  where
    getArity :: Tp t -> Int
    getArity t = case t of
      FunT _ _ x -> 1 + getArity x
      _ -> 0

    getNames :: Tp t -> [String]
    getNames t = case t of
      IntT _ -> []
      BoolT _ -> []
      ClassT _ (ClsNm x) -> [x]
      FunT _ t1 t2 -> getNames t1 ++ getNames t2 -- handle tree recursion in leaves
      TupleT _ tps -> concatMap getNames tps     -- handle tree recursion in leaves
      _ -> []

nlg, nlgAST :: Show t => GFlang -> FilePath -> Program t -> IO ()
nlg = nlg' False
nlgAST = nlg' True



nlg' :: Show t => Bool -> GFlang -> FilePath -> Program t -> IO ()
nlg' showAST gfl fpath prog = do
    gr <- createGF fpath prog
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

type CuteCats t = Reader (Env t)

data Env t
  = Env
      { lexicon :: [Mapping t],
        vardecls :: [[VarDecl t ]]
      }

program2prop :: Show t => Program t -> [GProp]
program2prop prg = 
    let lexc = lexiconOfProgram prg
        vars = globalsOfProgram prg
        rules = rulesOfProgram prg
        env0 = Env {lexicon = lexc, vardecls = [vars]}
     in runReader
          (mapM rule2prop rules)
          env0

vardecl2prop :: (HasDefault t, Show t) => VarDecl t -> CuteCats t GProp
vardecl2prop (VarDecl _ vname vtyp) = do
  typ <- tp2kind (GlobalVar (QVarName defaultVal vname)) vtyp
  name <- var2ind (GlobalVar (QVarName defaultVal vname))
  pure $ GPAtom (GAKind typ name)

var2ind :: Var t -> CuteCats t GInd
var2ind var = do
  let name =  nameOfQVarName (nameOfVar var)
  lex <- asks lexicon
  return $ case findMapping lex name of
     _:_  -> GIVarN (GAtomNoun (LexAtom name)) -- the var is in lexicon
     _    -> GIVar (GVString (GString name)) -- Fall back to string literal

var2pred :: Var t -> CuteCats t GPred1
var2pred var = do
  let name = nameOfQVarName (nameOfVar var)
  return $ GAtomPred1 (LexAtom name)

var2pred2 :: Var t -> CuteCats t GPred2
var2pred2 var = do
  let name = nameOfQVarName (nameOfVar var)
  return $ GAtomPred2 (LexAtom name)

tp2kind :: Show t => Var t -> Tp t -> CuteCats te GKind
tp2kind v e = case e of
  BoolT _ -> pure GBoolean
  IntT _ -> pure GNat
  ClassT _ (ClsNm name) -> pure $ GKNoun (var2quant v) (GAtomNoun (LexAtom name))
  FunT _ arg ret -> GKFun <$> tp2kind v arg <*> tp2kind v ret
  -- TupleT [Tp]
  -- ErrT
  _ -> error $ "tp2kind: not yet supported: " ++ show e

tp2ind :: Var t -> Tp t -> CuteCats te GInd
tp2ind v e = case e of
  --BoolT -> pure GBoolean
  --IntT -> pure GNat
  ClassT _ (ClsNm name) -> pure $ GINoun (var2quant v) (GAtomNoun (LexAtom name))
  --FunT arg ret -> GKFun <$> tp2kind arg <*> tp2kind ret
  -- TupleT [Tp]
  -- ErrT
  _ -> pure $ GIVar (GVString (GString "<unsupported>"))
  -- _ -> error $ "tp2kind: not yet supported: " ++ show e


var2quant :: Var t -> GQuantifier
var2quant = GQString . GString . varName

rule2prop :: Show t => Rule t -> CuteCats t GProp
rule2prop r =
  let r'@(Rule _ nm _ vars ifE thenE) = normalizeQuantifGF r in local (updateVars vars) $
  do
    ifProp <- expr2prop ifE
    thenProp <- expr2prop thenE
    return $ GPImpl ifProp thenProp

expr2prop :: Show t => Syntax.Expr t -> CuteCats t GProp
expr2prop e = case e of
  ValE _ val -> pure $ GPAtom (val2atom val)
  FunApp1 f x xTp -> do
    f' <- var2pred f
--    x' <- tp2ind xTp
    x' <- var2ind x
    pure $ GPAtom (GAPred1 f' x')
  FunApp2 f x xTp y yTp -> do
    f' <- var2pred2 f
--    x' <- tp2ind x xTp      -- MS: changed in analogy to FunApp1
    x' <- var2ind x
--    y' <- tp2ind y yTp      -- MS: changed in analogy to FunApp1
    y' <- var2ind y
    pure $ GPAtom (GAPred2 f' x' y')
  Exist x exp -> do
    prop <- expr2prop exp
    typ <- tp2kind (LocalVar (QVarName (annotOfVarDecl x) (nameOfVarDecl x)) 0) (tpOfVarDecl x)
    pure $ GPExists (GListVar [GVString (GString (nameOfVarDecl x))]) typ prop
  Forall x exp -> do
    prop <- expr2prop exp
    typ <- tp2kind (LocalVar (QVarName (annotOfVarDecl x) (nameOfVarDecl x)) 0) (tpOfVarDecl x)
    pure $ GPUnivs (GListVar [GVString (GString (nameOfVarDecl x))]) typ prop
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

val2atom :: Val -> GPropAtom
val2atom e = case e of
  BoolV True -> GAKind GBoolean GBTrue
  BoolV False -> GAKind GBoolean GBFalse
  IntV i -> GAKind GNat (GIInt (GInt (fromInteger i)))
  _ -> error $ "val2atom: not yet supported: " ++ show e

----------------------------------------
-- Patterns

pattern AppU :: Syntax.Expr t -> Syntax.Expr t -> Syntax.Expr t
pattern AppU x y <- AppE _ x y

pattern VarU :: Var t -> t -> Syntax.Expr t
pattern VarU var tp <- VarE tp var

pattern FunApp1 :: Var t -> Var t -> t -> Syntax.Expr t
pattern FunApp1 f x xTp <- AppU (VarU f _) (VarU x xTp)

-- AppU (VarU (GlobalVar f)) (VarU (LocalVar x int))

pattern FunApp2 :: Var t -> Var t ->  t -> Var t ->  t -> Syntax.Expr t
pattern FunApp2 f x xTp y yTp <- AppU (FunApp1 f x xTp) (VarU y yTp)

-- Quantification

pattern Exist :: VarDecl t -> Syntax.Expr t -> Syntax.Expr t
pattern Exist x exp <- QuantifE _ Ex x exp

pattern Forall :: VarDecl t -> Syntax.Expr t -> Syntax.Expr t
pattern Forall x exp <- QuantifE _ All x exp

-- Binary operations

pattern And :: Syntax.Expr t -> Syntax.Expr t -> Syntax.Expr t
pattern And e1 e2 <- BinOpE _ (BBool BBand) e1 e2

pattern Or :: Syntax.Expr t -> Syntax.Expr t -> Syntax.Expr t
pattern Or e1 e2 <- BinOpE _ (BBool BBor) e1 e2

pattern Not :: Syntax.Expr t -> Syntax.Expr t
pattern Not e <- UnaOpE _ (UBool UBnot) e

pattern Impl :: Syntax.Expr t -> Syntax.Expr t -> Syntax.Expr t
pattern Impl e1 e2 <- BinOpE _ (BBool BBimpl) e1 e2

pattern IfThenElse :: Syntax.Expr t -> Syntax.Expr t -> Syntax.Expr t -> Syntax.Expr t
pattern IfThenElse e1 e2 e3 <- IfThenElseE _ e1 e2 e3

----------------------------------------
-- Generic helper functions

updateVars :: [VarDecl t] -> Env t -> Env t
updateVars vs env = env {vardecls = vs : vardecls env}

findMapping :: [Mapping t] -> String  -> [String]
findMapping haystack needle =
  [ val
    | Mapping _ name (Descr val _) <- haystack,
      name == needle
  ]

type Lang = String

createLexicon :: [Lang] -> [Mapping t] -> (String , [String])
createLexicon langs lexicon = (abstract, concretes)
  where
    abstract =
      unlines $
        ["abstract PropLexicon = Prop ** {"]
          ++ ["fun"]
          ++ [ printf "%s : %s ;" name (gfType val)
               | Mapping _ name (Descr val _) <- lexicon
             ]
          ++ ["}"]
    concretes =
      [ unlines $
          [printf "concrete PropLexicon%s of PropLexicon = Prop%s ** open WordNet%s, Paradigms%s, Syntax%s, Extend%s in {" lang lang lang lang lang lang]
            ++ ["lin"]
            ++ [ printf "%s = %s ;" name val
                 | Mapping _ name (Descr val _) <- lexicon
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
