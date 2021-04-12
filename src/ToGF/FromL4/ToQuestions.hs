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

import Questions
import Syntax
import qualified GF
import PGF
import System.Environment (withArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)
import ToGF.GenerateLexicon hiding (grName, topName, lexName, createPGF, abstractLexicon, concreteLexicon)

import ToGF.FromSCasp.SCasp

-- lexicon layout

grName, topName, lexName :: Doc () --String
grName = "Question"
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

removeDupes :: [POS] -> [POS]
removeDupes (x:xs)
  | null xs = [x]
  | x == head xs = removeDupes xs
  | otherwise = x : removeDupes xs

--

getPred :: [VarDecl t] -> [GPred]
getPred (x:xs)
  | isPred x = toPred x : getPred xs
  | not (isPred x) = getPred xs
  | otherwise = []

printGF :: Gf a => PGF -> a -> IO ()
printGF gr expr = do
  --putStrLn $ showExpr [] $ gf expr
  mapM_ putStrLn (linearizeAll gr (gf expr))

-- grab atoms and names 
-- apply list of functions to get atoms
grabName :: GPred -> [GName]
grabName (GMkPred1 x _)  = [x]
grabName (GMkPred2 x _ _) = [x]

grabArgs :: GPred -> [GAtom]
grabArgs (GMkPred1 _ arg) = [arg]
grabArgs (GMkPred2 _ arg1 arg2) = [arg1, arg2]


--data POS = POS {origName :: String, pos :: InnerPOS}

--data InnerPOS = PN2 String Prep | PN String | PV2 String Prep | PV String

--Not found in lexicon: create the N/V/A/N2/V2/A2 using GF smart paradigms
--mkName (rock_1_N)
--mkAtom (mkV2 throw_2_V)
--mkName (mkV2 (mkV "foo"))
--mkAtom (mkN "bar")

makeArgPOS :: GAtom -> POS
makeArgPOS (LexAtom x) = POS x $ PN getStr
  where getStr
         | isDigit (last x) = init x
         | otherwise = x

getListPOS :: [GAtom] -> [POS]
getListPOS = map makeArgPOS

--getPOS :: [GAtom] -> POS
--getPOS (x:xs)
-- | getLength == 2 =  
-- | getLength == 1 = singlePOS x 
-- where
--   getLength = length (x:xs)

mkQLexicon :: [GPred] -> (Doc (), Doc ())
mkQLexicon x = (abstractLexicon $ removeDupes lexicon, concreteLexicon $ removeDupes lexicon)
  where
    lexicon = getListPOS $ concatMap grabArgs x
    --lexicon = map makeArgPOS $ grabArgs x

createPredGF :: [GPred] -> IO ()
createPredGF x  = do
  let (absS, cncS) =  mkQLexicon x
  print "check abs"
  print absS
  print "check concrete"
  print cncS
  writeDoc (mkAbsName lexName) absS
  writeDoc (mkCncName lexName) cncS
  writeDoc (mkAbsName topName) $ "abstract " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "," <+> lexName <+> "** {flags startcat = Statement ;}"
  writeDoc (mkCncName topName) $ "concrete " <> topName <> "Eng of " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "Eng," <+> lexName <> "Eng ;"

--createGF for all [GPred]
nlgPreds :: [GPred] -> IO ()
nlgPreds [] = error "No preds"
nlgPreds ls = do
  createPredGF ls -- dump all preds 
  gr <- createPGF
  --let allAtoms = concatMap grabArgs ls
  print "hello"

hello :: Program Tp -> IO ()
hello prog = do
  nlgPreds $ map toPred $ filter isPred (globalsOfProgram prog)
  mapM_ (putStrLn . showExpr [] . gf) (toQuestions prog)

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