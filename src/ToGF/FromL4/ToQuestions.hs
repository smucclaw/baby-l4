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
grabNames :: [GPred] -> [GName]
grabNames x = [getName,getName2] <*> x

grabArgs :: [GPred] -> [GAtom]
grabArgs x = [getArg, getArg1, getArg2] <*> x

--data POS = POS {origName :: String, pos :: InnerPOS}

--data InnerPOS = PN2 String Prep | PN String | PV2 String Prep | PV String


setPOS :: GAtom -> POS 
setPOS (LexAtom str) = POS str $ case str of
  [noun] -> PN noun


mkQLexicon :: GPred -> (Doc (), Doc ())
mkQLexicon x = (abstractLexicon lexicon, concreteLexicon lexicon)
  where
    lexicon = setPOS <$> S.toList (grabArgs x)

createGF :: GPred -> IO ()
createGF x  = do
  let (absS, cncS) = mkQLexicon x
  writeDoc (mkAbsName lexName) absS
  writeDoc (mkCncName lexName) cncS
  writeDoc (mkAbsName topName) $ "abstract " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "," <+> lexName <+> "** {flags startcat = Statement ;}"
  writeDoc (mkCncName topName) $ "concrete " <> topName <> "Eng of " <> topName <+> "=" <+> ToGF.FromL4.ToQuestions.grName <> "Eng," <+> lexName <> "Eng ;"

nlg :: GPred -> IO ()
nlg x = do
  ToGF.FromL4.ToQuestions.createGF x

--dumpModels :: [Model] -> Model
--dumpModels = MExps . foldMap getModel
--  where
--    getModel :: Model -> [Exp]
--    getModel (MExps es) = es

--

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