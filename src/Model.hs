
module Model where

import SimpleSMT as SMT
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, nub)


-- the model to be constructed from an SMT model:
-- association of a string (relation name) with
-- a list (length: arity of the relation) of model elements
data RelModelElem
    = RMEBool Bool
    | RMEInt Int
    | RMEString String
    | RMESExpr SExpr
type RelModel = [(String, [RelModelElem])]

-- bindings used during SExpr translation: name of a parameter, expression it is bound to
type Argmap = [(String, SMT.SExpr)]

smtTrue  = Atom "true"
smtFalse = Atom "false"

simpSmtNot :: SMT.SExpr -> SMT.SExpr
simpSmtNot (Atom "true") = smtFalse
simpSmtNot (Atom "false") = smtTrue
simpSmtNot (List [Atom "not", e]) = e
simpSmtNot e = List [Atom "not", e]

simpSmtImpl :: SExpr -> SExpr -> SExpr
simpSmtImpl (Atom a) e = if a == "false" then smtFalse else e
simpSmtImpl e (Atom a) = if a == "false" then smtFalse else e
simpSmtImpl e1 e2 = if e1 == e2 then smtTrue else List [Atom "=>", e1, e2]

simpSmtAnd :: SExpr -> SExpr -> SExpr
simpSmtAnd (Atom a) e = if a == "false" then smtFalse else e
simpSmtAnd e (Atom a) = if a == "false" then smtFalse else e
simpSmtAnd e1 e2 = if e1 == e2 then e1 else List [Atom "and", e1, e2]

simpSmtOr :: SExpr -> SExpr -> SExpr
simpSmtOr (Atom a) e = if a == "true" then smtTrue else e
simpSmtOr e (Atom a) = if a == "true" then smtTrue else e
simpSmtOr e1 e2 = if e1 == e2 then e1 else List [Atom "or", e1, e2]

simpSmtEqual :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
simpSmtEqual e1@(Atom a1) e2@(Atom a2)
  | a1 == a2 = smtTrue
  | isIntString a1 || isIntString a2 = List [Atom "=", e1, e2]
  | otherwise = smtFalse
simpSmtEqual e1 e2 = if e1 == e2 then smtTrue else List [Atom "=", e1, e2]

-- TODO: do the same as above
simpSmtDistinct :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
simpSmtDistinct (Atom a1) (Atom a2) = if a1 == a2 then smtFalse else smtTrue
simpSmtDistinct e1 e2 = if e1 == e2 then smtFalse else List [Atom "distinct", e1, e2]

simpSmtIte :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
simpSmtIte (Atom a) et ee = if a == "true" then et else ee
simpSmtIte ec et ee = if et == ee then et else List [Atom "ite", ec, et, ee]

simpSmtUnInts :: String -> SMT.SExpr -> SMT.SExpr
simpSmtUnInts "-" (Atom i) = Atom (show (- (read i)))
simpSmtUnInts "-" e = List [Atom "-", e]
simpSmtUnInts s e = error ("simpSmtExpr: unary operator " ++ s ++ " not supported")

smtBinopArith :: Num a => String -> a -> a -> a
smtBinopArith "-" = (-)
smtBinopArith "+" = (+)
smtBinopArith "*" = (*)

smtBinopCompar :: String -> Int -> Int -> Bool
smtBinopCompar "<=" = (<=)
smtBinopCompar "<" = (<)
smtBinopCompar ">=" = (>=)
smtBinopCompar ">" = (>)

isIntString :: String -> Bool
isIntString = all isDigit

simpSmtBinInts :: String -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
simpSmtBinInts binop e1@(Atom i1) e2@(Atom i2) =
    if isIntString i1 && isIntString i2
    then if binop `elem` ["-", "+", "*"]
         then Atom (show (smtBinopArith binop (read i1) (read i2)))
         else SMT.bool (smtBinopCompar binop (read i1) (read i2))
    else List [Atom binop, e1, e2]
simpSmtBinInts s e1 e2 = error ("simpSmtExpr: binary operator " ++ s ++ " not supported")

-- see the definition of theory Core, theory Ints in the SMT-LIB standard
simpSmtExpr :: Argmap -> SMT.SExpr -> SMT.SExpr
simpSmtExpr argm (Atom "true") = smtTrue
simpSmtExpr argm (Atom "false") = smtFalse
simpSmtExpr argm a@(Atom x) = fromMaybe a (lookup x argm)
simpSmtExpr argm (List [Atom "not", e]) = simpSmtNot (simpSmtExpr argm e)
simpSmtExpr argm (List [Atom "=>", e1, e2]) = simpSmtImpl (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "and"]) = smtTrue
simpSmtExpr argm (List [Atom "and", e1]) = simpSmtExpr argm e1
simpSmtExpr argm (List [Atom "and", e1, e2]) = simpSmtAnd (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List ((Atom "and") : e1 : es)) =
    simpSmtAnd (simpSmtExpr argm e1) (simpSmtExpr argm (List (Atom "and" : es)))
simpSmtExpr argm (List [Atom "or"]) = smtFalse
simpSmtExpr argm (List [Atom "or", e1]) = simpSmtExpr argm e1
simpSmtExpr argm (List [Atom "or", e1, e2]) = simpSmtOr (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List ((Atom "or") : e1 : es)) =
    simpSmtOr (simpSmtExpr argm e1) (simpSmtExpr argm (List (Atom "or" : es)))
simpSmtExpr argm (List [Atom "xor", e1, e2]) = simpSmtDistinct (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "=", e1, e2]) = simpSmtEqual (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "distinct", e1, e2]) = simpSmtDistinct (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "ite", e1, e2, e3]) = simpSmtIte (simpSmtExpr argm e1) (simpSmtExpr argm e2)  (simpSmtExpr argm e3)
simpSmtExpr argm (List [Atom "-", e]) = simpSmtUnInts "-" (simpSmtExpr argm e)
simpSmtExpr argm (List [Atom "-", e1, e2]) = simpSmtBinInts  "-" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "+", e1, e2]) = simpSmtBinInts  "+" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "*", e1, e2]) = simpSmtBinInts  "*" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "<=", e1, e2]) = simpSmtBinInts  "<=" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "<", e1, e2]) = simpSmtBinInts  "<" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom ">=", e1, e2]) = simpSmtBinInts  ">=" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom ">", e1, e2]) = simpSmtBinInts  ">" (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "let", List [], e]) = simpSmtExpr argm e
simpSmtExpr argm (List [Atom "let", List (List [Atom vn, le]:les), e]) =
    simpSmtExpr ((vn, simpSmtExpr argm le):argm) (List [Atom "let", List les, e])



foobar :: [Integer] -> [[Integer]] -> [[Integer]]
-- foobar ps pps = map  (\p -> concatMap (p :) pps)  ps
foobar ps pps = map (:) ps <*> pps


bindSmt :: [SMT.SExpr] -> [SMT.SExpr] -> Argmap
bindSmt params = zip (map (\(List [ Atom vn, tp]) -> vn) params)

instAndSimpFun :: [SMT.SExpr] -> SMT.SExpr -> SMT.SExpr
instAndSimpFun args (List [Atom "define-fun" , Atom fn, List params, tp, bd]) =
    simpSmtExpr (bindSmt params args) bd

isInstance :: SMT.SExpr -> Bool
isInstance (List [ Atom "declare-fun", Atom fn, List [], Atom tp]) = True
isInstance _ = False

isDefFunOfSort :: (String -> Bool) -> SMT.SExpr -> Bool
isDefFunOfSort srttest (List [ Atom "define-fun", Atom fn, List params, Atom srt, bd]) = srttest srt
isDefFunOfSort _ _ = False

-- list of quadruples: function name, parameters, sort, body of function
defFunDecompose :: SExpr -> (String, [SExpr], String, SExpr)
defFunDecompose (List [ Atom "define-fun", Atom fn, List params, Atom srt, bd]) = (fn, params, srt, bd)

extractInstance :: SMT.SExpr -> (String, String)
extractInstance (List [ Atom "declare-fun", Atom fn, List [], Atom tp]) = (fn, tp)

-- returns a list of pairs (instance name, sort)
modelInstances :: SMT.SExpr -> [(String, String)]
modelInstances (List (Atom "model" : es)) = map extractInstance (filter isInstance es)

modelPreds :: SMT.SExpr -> [(String, [SExpr], String, SExpr)]
modelPreds (List (Atom "model" : es)) = map defFunDecompose (filter (isDefFunOfSort (== "Bool")) es)

modelConsts :: SMT.SExpr -> [(String, [SExpr], String, SExpr)]
modelConsts (List (Atom "model" : es)) =
    filter (\(fn, params, srt, bd) -> null params) (map defFunDecompose (filter (isDefFunOfSort (/= "Bool")) es))

constructCurrentBinding :: [(String, [String])] -> Int -> String -> String -> [(String, SMT.SExpr)]
constructCurrentBinding instsBySort n pn ps =
    case ps of
        "Bool" -> [(pn, smtTrue), (pn, smtFalse)]
        "Int" -> [(pn, Atom ("intArg"++show n))]
        _ -> maybe [] (map (\i -> (pn, Atom i))) (lookup ps instsBySort)

constructBindings :: [(String, [String])] -> Int -> [SExpr] -> [Argmap]
constructBindings instsBySort n [] = [[]]
constructBindings instsBySort n ((List [Atom pn, Atom ps]):params) =
    map (:) (constructCurrentBinding instsBySort n pn ps) <*> constructBindings instsBySort (n+1) params

bindingsOfPred :: [(String, [String])] -> (String, [SExpr], String, SExpr) -> (String, [Argmap], SExpr)
bindingsOfPred instsBySort (fn, params, _, bd) = (fn, constructBindings instsBySort 0 params, bd)

-- constructModel :: SMT.SExpr -> RelModel
-- constructModel :: SExpr -> [(String, [Argmap])]
constructModel smtmd =
    let instances = modelInstances smtmd
        sorts = nub (map snd instances)
        instsBySort = map (\s -> (s, map fst (filter (\(i, si) -> s == si) instances))) sorts
        preds = modelPreds smtmd
        binds = map (bindingsOfPred instsBySort) preds
        mds = concatMap (\(fn, argms, bd) -> map (\argm -> (fn, argm, simpSmtExpr argm bd)) argms) binds
    in  mds


{-| 

>>> simpSmtExpr [("x!3",Atom "intArg3")] (List [ Atom "=" , Atom "x!3" , Atom "130" ])
WAS WAS WAS WAS WAS WAS Atom "false"
WAS WAS WAS WAS WAS NOW Atom "true"
WAS WAS WAS WAS NOW Atom "false"
WAS WAS WAS NOW Atom "false"
WAS WAS NOW Atom "false"
WAS NOW List [Atom "=",Atom "intArg3",Atom "130"]
NOW List [Atom "=",Atom "intArg3",Atom "130"]

>>> simpSmtBinInts [("x!0",Atom "Class!val!1"),("x!1",Atom "Class!val!1"),("x!2",Atom "Class!val!1"),("x!3",Atom "intArg3")] (List [ Atom "=" , Atom "x!3" , Atom "130" ])
WAS WAS WAS False
WAS WAS NOW True
WAS NOW Couldn't match expected type ‘Char’
WAS NOW             with actual type ‘([Char], SExpr)’
WAS NOW Couldn't match expected type ‘Char’
WAS NOW             with actual type ‘([Char], SExpr)’
WAS NOW Couldn't match expected type ‘Char’
WAS NOW             with actual type ‘([Char], SExpr)’
WAS NOW Couldn't match expected type ‘Char’
WAS NOW             with actual type ‘([Char], SExpr)’
NOW Couldn't match expected type ‘Char’
NOW             with actual type ‘([Char], SExpr)’
NOW Couldn't match expected type ‘Char’
NOW             with actual type ‘([Char], SExpr)’
NOW Couldn't match expected type ‘Char’
NOW             with actual type ‘([Char], SExpr)’
NOW Couldn't match expected type ‘Char’
NOW             with actual type ‘([Char], SExpr)’

-}
