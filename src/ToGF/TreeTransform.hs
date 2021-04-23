{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module ToGF.TreeTransform where

import Data.List.Extra (groupBy, partition)
import PGF (showExpr)
import Answer

----------------------------------------------------------------------
-- GF tree transformations

{-
firstAggr gets this as argument:
* RPS is a game , 
* A participates in RPS , 
* A plays , 
* A throws rock , 
* C plays , 
* C participates in RPS , 
* C throws scissors and 
* rock beats scissors

groupBy' samePred statements returns this:

[[RPS is a game] , 
[A participates in RPS , C participates in RPS] ,
[A plays               , C plays ], 
[A throws rock] , 
[C throws scissors],
[rock beats scissors]]

aggregateSubj transforms that list of lists into

* RPS is a game , 
* A and C participate in RPS , 
* A and C play , 
* A throws rock , 
* C throws scissors and 
* rock beats scissors

groupBy sameSubj returns this:

[
[ RPS is a game ], 
[ A and C participate in RPS , A and C play ], 
[ A throws rock ], 
[ C throws scissors], 
[ rock beats scissors]]

aggregatePred transforms that list of lists into

* RPS is a game , 
* A and C play and participate in RPS , 
* A throws rock , 
* C throws scissors and
* rock beats scissors
-}

aggregate :: [GStatement] -> [GStatement]
aggregate statements = secondAggr
  where
    firstAggr =
      [ case grp of
          [] -> error "aggregate: empty list"
          [x] -> x
          x : _ -> aggregateSubj (map getSubj grp) x
        | grp <- groupBy' samePred statements
      ]
    secondAggr =
      concat
        [ case grp of -- grp = [ A and C participate in RPS , A and C play ],
            [] -> error "aggregate: empty list"
            [x] -> [x]
            x : _ ->
              case aggregatePred (map getPred grp) x of -- preds = [participate, play]
                resultStatement
                  | x == resultStatement -> grp -- aggregatePred didn't do anything :(
                  | otherwise -> [resultStatement]
          | grp <- groupBy sameSubj firstAggr --NB. this is the standard groupBy! We know that "A and C" statements are already next to each other
        ]


aggregateSubj :: [GArg] -> GStatement -> GStatement
aggregateSubj subjs (GApp1 pr _subj) = GAggregateSubj1 pr (GListArg subjs)
aggregateSubj subjs (GApp2 pr _subj obj) = GAggregateSubj2 pr obj (GListArg subjs)
aggregateSubj _ x = x

aggregatePred :: [GPred] -> GStatement -> GStatement
aggregatePred [pr1, pr2] (GAggregateSubj1 _ subjs) = GAggregatePred pr1 pr2 subjs
aggregatePred [pr1, pr2] (GAggregateSubj2 _ _ subjs) = GAggregatePred pr1 pr2 subjs
aggregatePred _ x = x

getPred :: GStatement -> GPred
getPred s = case s of
  GApp1 pr _ -> GIntransPred pr
  GApp2 pr arg _ -> GTransPred pr arg
  GAggregateSubj1 pr _ -> GIntransPred pr
  GAggregateSubj2 pr arg _ -> GTransPred pr arg
  _ -> error $ "getPred applied to a complex tree " ++ showExpr [] (gf s)

samePred :: Answer.Tree a -> Answer.Tree a -> Bool
samePred s1 s2 = ignoreSubj s1 == ignoreSubj s2

sameSubj :: Answer.Tree a -> Answer.Tree a -> Bool
sameSubj s1 s2 = ignorePred s1 == ignorePred s2

getSubj :: GStatement -> GArg
getSubj s = case s of
  GApp1 _ subj -> subj
  GApp2 _ subj _ -> subj
  _ -> error $ "getSubj applied to a complex tree " ++ showExpr [] (gf s)

ignoreSubj :: Answer.Tree a -> Answer.Tree a
ignoreSubj s = case s of
  GApp1 pr _ -> GApp1 pr dummyArg
  GApp2 pr _ obj -> GApp2 pr dummyArg obj
  _ -> composOp ignoreSubj s


ignorePred :: Answer.Tree a -> Answer.Tree a
ignorePred s = case s of
  GApp1 _ subj -> GApp2 dummyAtom subj dummyArg
  GApp2 _ subj _ -> GApp2 dummyAtom subj dummyArg
  GAggregateSubj1 _ subjs -> GAggregateSubj2 dummyAtom dummyArg subjs
  GAggregateSubj2 _ _ subjs -> GAggregateSubj2 dummyAtom dummyArg subjs
  _ -> composOp ignorePred s

dummyAtom :: GAtom
dummyAtom = LexAtom "dummy"

dummyArg :: GArg
dummyArg = GAVar (GV (GString "dummy"))

-- from https://mail.haskell.org/pipermail/haskell-cafe/2014-March/113271.html
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f (a : rest) = (a : as) : groupBy' f bs
  where
    (as, bs) = partition (f a) rest

