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

import Questions
import Syntax
import PGF
import ToGF.GenerateLexicon
import ToGF.FromSCasp.SCasp

-- to lexicon

grName :: Doc () --String
grName = "Question"

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
getName :: GPred -> GName
getName (GMkPred1 x _)  = x

getArg :: GPred -> GAtom
getArg (GMkPred1 _ x)  = x

getName2 :: GPred -> GName
getName2 (GMkPred2 x _ _)  = x

getArg1 :: GPred -> GAtom
getArg1 (GMkPred2 _ x _)  = x

getArg2 :: GPred -> GAtom
getArg2 (GMkPred2 _ _ x)  = x

-- apply list of functions to get atoms
grabNames :: GPred -> [GName]
grabNames x = map ($ x) [getName,getName2]

grabArgs :: GPred -> [GAtom]
grabArgs x = map ($ x) [getArg, getArg1, getArg2]

--data POS = POS {origName :: String, pos :: InnerPOS}

--data InnerPOS = PN2 String Prep | PN String | PV2 String Prep | PV String

--Not found in lexicon: create the N/V/A/N2/V2/A2 using GF smart paradigms
--mkName (rock_1_N)
--mkAtom (mkV2 throw_2_V)
--mkName (mkV2 (mkV "foo"))
--mkAtom (mkN "bar")

makeArgPOS :: GAtom -> POS
makeArgPOS (LexAtom x) = POS x $ PN getStr
  where getNum
         | isDigit (last x) = digitToInt (last x)
         | otherwise = 0
        getStr = init x

--getPOS :: [GAtom] -> POS
--getPOS (x:xs)
-- | getLength == 2 =  
-- | getLength == 1 = singlePOS x 
-- where
--   getLength = length (x:xs)

mkQLexicon :: GPred -> (Doc (), Doc ())
mkQLexicon x = (abstractLexicon lexicon, concreteLexicon lexicon)
  where
    lexicon = map makeArgPOS (grabArgs x)

-- createGF for single GPred

createPredGF :: GPred -> IO ()
createPredGF x  = do
  let (absS, cncS) = mkQLexicon x
  writeDoc (mkAbsName lexName) absS
  writeDoc (mkCncName lexName) cncS
  writeDoc (mkAbsName topName) $ "abstract " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "," <+> lexName <+> "** {flags startcat = Statement ;}"
  writeDoc (mkCncName topName) $ "concrete " <> topName <> "Eng of " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "Eng," <+> lexName <> "Eng ;"

--createGF for all [GPred]
nlgPreds :: [GPred] -> IO ()
nlgPreds [] = error "No preds"
nlgPreds ls = do
  let allPred = mapM_ createPredGF ls
  allPred -- dump all preds 
  gr <- createPGF
  let allAtoms = concatMap grabArgs ls
  --printGF gr allAtoms
  print "hello"

hello :: Questionable x => x -> IO ()
hello prog =
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