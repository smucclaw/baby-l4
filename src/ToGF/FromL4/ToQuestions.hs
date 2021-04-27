{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.FromL4.ToQuestions where

import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.List.Extra (splitOn)

import Questions
import Syntax
import qualified GF
import PGF
import System.Environment (withArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)
import ToGF.GenerateLexicon hiding (origName, POS, grName, topName, lexName, createPGF, abstractLexicon, concreteLexicon)

import ToGF.FromL4.ToProp

-- lexicon layout


data POS = POS {origName :: String, pos :: String}
  deriving (Eq, Show)

grName, topName, lexName :: Doc () --String
grName = "Questions"
topName = grName <> "Top"
lexName = grName <> "Lexicon"

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

concreteLexicon :: [POS] -> Doc ()
concreteLexicon poses =
  vsep
    [ "concrete" <+> lexName <> "Eng of" <+> lexName <+> "=" <+> "Atoms" <> "Eng ** open SyntaxEng, ParadigmsEng, WordNetEng in {",
      "lin",
      (indent 4 . vsep) (eachConcrete <$> poses),
      "}"
    ]

eachConcrete :: POS -> Doc ()
eachConcrete (POS name lex) = hsep [pretty name, "=", "mkAtom", parens $ pretty lex, ";"]

abstractLexicon :: [POS] -> Doc ()
abstractLexicon poses =
  vsep
    [ "abstract" <+> lexName <+> "=" <+> "Atoms" <+> "** {",
      "fun",
      indent 4 . sep . punctuate "," . map (pretty . origName) $ poses,
      indent 4 ": Atom ;",
      "}"
    ]

--

getPred :: [VarDecl t] -> [GPred]
getPred (x:xs)
  | isPred x = toPred x : getPred xs
  | not (isPred x) = getPred xs
  | otherwise = []

printGF :: Gf a => PGF -> a -> IO ()
printGF gr expr =
  mapM_ putStrLn (linearizeAll gr (gf expr))

-- grab atoms and names 
-- apply list of functions to get atoms

grabNames :: GPred -> [GName]
grabNames (GMkPred1 x _)  = [x]
grabNames (GMkPred2 x _ _) = [x]

grabArgs :: GPred -> [GAtom]
grabArgs (GMkPred1 _ arg) = [arg]
grabArgs (GMkPred2 _ arg1 arg2) = [arg1, arg2]

--Not found in lexicon: create the N/V/A/N2/V2/A2 using GF smart paradigms
--mkAtom (mkV2 throw_2_V)
--mkAtom (mkN "bar")

-- makeNamePOS :: GName -> POS
-- makeNamePOS (LexName x) = POS x $ PN x

-- makeArgPOS :: GAtom -> POS
-- makeArgPOS (LexAtom x) = POS x $ PN x

-- makeNamePOS :: GName -> Mapping t -> POS
-- makeNamePOS (LexName x) (Mapping _ name gfexpr) =
--   POS x $ matchNames x (grabLexicon ())

-- getListNamePOS :: [GName] -> [POS]
-- getListNamePOS = map makeNamePOS

-- getListAtomPOS :: [GAtom] -> [POS]
-- getListAtomPOS = map makeArgPOS

--grabLexicon prog = lexiconofProgram prog
-- Mapping _ _ gfexp

-- get gftype from lexicon

grabLexicon :: Mapping t -> (String, String)
grabLexicon (Mapping _ name gfexpr) = (name, gfexpr)

-- just generate Lexicon

-- get prog to [gname], 
getNameFromProg prog = map (grabNames . toPred) (globalsOfProgram prog)

-- prog to [mapping t's (string, string)]
getGFExprFromProg :: Program Tp -> [(String, String)]
getGFExprFromProg prog = map grabLexicon (lexiconOfProgram prog)

-- rock_1_N
-- assignPOS to atoms
assignPOS :: (String, String) -> POS
assignPOS x = POS (fst x) $ checkWords (snd x)

checkWords x
  | length str == 1 = x
  | length str > 1 = head str ++ " (" ++ unwords (tail str) ++ ")"
  where str = words x
  -- POS (fst x) $ case concatMap (splitOn "_") (splitOn " " (snd x)) of
  --   [word, _, "N"] -> PN word
  --   [word, _, "V2"] -> PV2 word Nothing
  --   [word, _, "V"] -> PV word
  --   [word, _, "N2"] -> PN2 word Nothing
  --   ["mkV2", word, _, _, prep, "Prep"] -> PV2 word (Just prep)
  --   _ -> error $ "error " ++ show x

makeProgLexicon :: Program Tp -> (Doc (), Doc ())
makeProgLexicon prog = (abstractLexicon lex, concreteLexicon lex)
  where
    lex = map assignPOS (getGFExprFromProg prog)

-- mkQLexicon :: [GPred] -> (Doc (), Doc ())
-- mkQLexicon x = (abstractLexicon lexicon, concreteLexicon lexicon)
--   where
--     lexicon =
--       nub $ getListAtomPOS (concatMap grabArgs x) ++ getListNamePOS (concatMap grabNames x)

createProgGF :: Program Tp -> IO ()
-- createPredGF :: [GPred] -> IO ()
createProgGF x  = do
  -- let (absS, cncS) =  mkQLexicon x
  let (absS, cncS) =  makeProgLexicon x
  writeDoc (mkAbsName lexName) absS
  writeDoc (mkCncName lexName) cncS
  writeDoc (mkAbsName topName) $ "abstract " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "," <+> lexName <+> "** {flags startcat = Question ;}"
  writeDoc (mkCncName topName) $ "concrete " <> topName <> "Eng of " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "Eng," <+> lexName <> "Eng ;"

--createGF for all [GPred]
-- nlgPreds :: [GPred] -> IO ()
-- nlgPreds [] = error "No preds"
-- nlgPreds ls = do
--   createPredGF ls -- dump all preds 
--   gr <- ToGF.FromL4.ToQuestions.createPGF
--   --let allAtoms = concatMap grabArgs ls
--   print "hello"

hello :: Program Tp -> IO ()
hello prog = do
  createProgGF prog
  -- nlgPreds $ map toPred $ filter isPred (globalsOfProgram prog)
  -- mapM_ (putStrLn . showExpr [] . gf) (toQuestions prog)

class Questionable x where
    toQuestions :: x -> [GQuestion]

instance Questionable (VarDecl t) where
  toQuestions v = [GAreThereAny, GAreThereMore,  GProperties] <*>  [toPred v]

instance Questionable (Program a) where
  toQuestions = concatMap toQuestions . filter isPred.globalsOfProgram


toPred :: VarDecl t -> GPred
toPred (Pred1 name arg1)      = GMkPred1 (LexName name) (LexAtom arg1)
toPred (Pred2 name arg1 arg2) = GMkPred2 (LexName name) (LexAtom arg1) (LexAtom arg2)
--toPred _ = error "error"

isPred :: VarDecl t -> Bool
isPred = isPred' . tpOfVarDecl

isPred' :: Tp -> Bool
isPred' (FunT t BoolT) = True
isPred' (FunT t t2) = isPred' t2
isPred' _ = False


--------------------
-- patterns

pattern Pred1 :: VarName -> String           -> VarDecl t
pattern Pred1 name arg1 <- VarDecl _ name (Arg1 arg1)  -- to create VarDecl String Tp

pattern Pred2 :: VarName -> String -> String -> VarDecl t
pattern Pred2 name arg1 arg2 <- VarDecl _ name (Arg2 arg1 arg2)

pattern Arg1 :: String -> Tp
pattern Arg1 x <- FunT (ClassT (ClsNm  x) ) _

pattern Arg2 :: String -> String -> Tp
pattern Arg2 x y <- FunT (ClassT (ClsNm  x) ) (Arg1 y)

{- (FunT
     IntT 
     (FunT IntT BoolT)) -}