{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.FromSCasp.AnswerToGF where

import GHC.Exts (the)
import PGF (PGF, linearizeAll)
import Answer
import ToGF.FromSCasp.SCasp as SC
import ToGF.GenerateLexicon
import ToGF.TreeTransform

-- 1) This is a data family that translates SKind to GF types
type family SKind2GF (k :: SKind) :: * where
  SKind2GF KModel = GStatement
  SKind2GF KExp = GStatement
  SKind2GF KArg = GArg
  SKind2GF KVar = GVar
  SKind2GF KAtom = GAtom

-- 2) All sCASP trees are of type Tree a: fewer functions needed
toGF :: SC.Tree a -> SKind2GF a
toGF (MExps ss) = unpeel $ toGF <$> ss
toGF (EApp f [x]) = GApp1 (toGF f) (toGF x)
toGF (EApp f [x, y]) = GApp2 (toGF f) (toGF x) (toGF y)
toGF (AAtom tk) = GAAtom (toGF tk)
toGF (AVar tk) = GAVar (toGF tk)
toGF (A str) = LexAtom str
toGF (V str) = GV (GString str)
toGF _ = undefined

-- temporary hack, to get something nice to print and get back the list
peel :: GStatement -> [GStatement]
peel (GConjStatement _ (GListStatement ss)) = ss
peel s = [s]

unpeel :: [GStatement] -> GStatement
unpeel = wrap GBullets

wrap :: GTypography -> [GStatement] -> GStatement
wrap t ss = case ss of
  [] -> error "wrap: empty list"
  [x] -> x
  _ -> GConjStatement t $ GListStatement ss

----------------------------------------------------------------------
-- print etc.

postprocess :: String -> String
postprocess = map (\c -> if c == '\\' then '\n' else c)

printGF :: Gf a => PGF -> a -> IO ()
printGF gr expr = do
  --putStrLn $ showExpr [] $ gf expr
  mapM_ (putStrLn . postprocess) (linearizeAll gr (gf expr))

nlgModels :: [Model] -> IO ()
nlgModels [] = error "nlgModels: no models given"
nlgModels [model] = nlg model -- default to nlg for just a single model
nlgModels models = do
  createGF (dumpModels models) -- We need all models together just to create lexicon
  gr <- createPGF
  let gfModels = toGF <$> models
  -- putStrLn "\nRaw translation of the model"
  -- mapM_ (printGF gr) gfModels

  let concls_evidences =
        [ (concl, aggregate evidence)
          | gfModel <- gfModels,
            let concl : evidence = peel gfModel
        ]
  let conclusion = the $ map fst concls_evidences
  let allEvidence = concatMap snd concls_evidences
  let shared = [s | ss@(s :_) <- groupBy' (==) allEvidence,
                   length ss == length concls_evidences ]
  let uniques =
        [ wrap
            GInline
            [ st
              | st <- evidence,
                st `notElem` shared
            ]
          | (_, evidence) <- concls_evidences
        ]
  ---------
  -- Final printout
  printGF gr conclusion
  putStrLn "\nif all of the following hold:"
  printGF gr $ unpeel shared
  putStrLn "\nand one of the following holds:"
  printGF gr $ GDisjStatement GBullets (GListStatement uniques)


nlg :: Model -> IO ()
nlg model = do
  createGF model
  gr <- createPGF
  let gfModel = toGF model
  putStrLn "\nRaw translation of the model"
  printGF gr gfModel
  let aggr@(f : rest) = aggregate $ peel gfModel
  -- putStrLn "\nFirst step: aggregation"
  -- printGF $ unpeel aggr

  let caus = GIfThen f (unpeel rest)
  putStrLn "\nAggregation and causality (relying on the original order)\n"
  printGF gr caus
