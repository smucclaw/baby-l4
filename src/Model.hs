
module Model where

import SimpleSMT as SMT ( bool, SExpr(..) )
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, nub, (\\))


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
  | (isIntString a1 || isIntString a2) && not (isIntString a1 && isIntString a2) = List [Atom "=", e1, e2]
  | otherwise = smtFalse
simpSmtEqual e1 e2 = if e1 == e2 then smtTrue else List [Atom "=", e1, e2]

simpSmtDistinct :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
simpSmtDistinct e1@(Atom a1) e2@(Atom a2)
  | a1 == a2 = smtFalse
  | (isIntString a1 || isIntString a2) && not (isIntString a1 && isIntString a2) = List [Atom "distinct", e1, e2]
  | otherwise = smtTrue
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
simpSmtBinInts binop e1 e2 = List [Atom binop, e1, e2]

-- see the definition of theory Core, theory Ints in the SMT-LIB standard
simpSmtExpr :: Argmap -> SMT.SExpr -> SMT.SExpr
simpSmtExpr argm (Atom "true") = smtTrue
simpSmtExpr argm (Atom "false") = smtFalse
simpSmtExpr argm a@(Atom x) = fromMaybe a (lookup x argm)
simpSmtExpr argm (List [Atom "not", e]) = simpSmtNot (simpSmtExpr argm e)
simpSmtExpr argm (List [Atom "=>", e1, e2]) = simpSmtImpl (simpSmtExpr argm e1) (simpSmtExpr argm e2)
simpSmtExpr argm (List [Atom "and"]) = smtTrue
simpSmtExpr argm (List ((Atom "and") : e1 : es)) =
    simpSmtAnd (simpSmtExpr argm e1) (simpSmtExpr argm (List (Atom "and" : es)))
simpSmtExpr argm (List [Atom "or"]) = smtFalse
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

isDeclareFun :: SExpr -> Bool
isDeclareFun (List [ Atom "declare-fun", Atom fn, args, Atom tp]) = True
isDeclareFun _ = False

isDefineFun :: SExpr -> Bool
isDefineFun (List [ Atom "define-fun", Atom fn, args, Atom tp, e]) = True
isDefineFun _ = False

fromModel :: SMT.SExpr -> [SMT.SExpr]
fromModel (List es) = filter (\e -> isDeclareFun e || isDefineFun e) es

-- returns a list of pairs (instance name, sort)
getInstances :: [SMT.SExpr] -> [(String, String)]
getInstances = map extractInstance . filter isInstance

getPreds :: [SMT.SExpr] -> [(String, [SExpr], String, SExpr)]
getPreds = map defFunDecompose . filter (isDefFunOfSort (== "Bool"))

getConsts :: [SMT.SExpr] -> [(String, [SExpr], String, SExpr)]
getConsts es =
    filter (\(fn, params, srt, bd) -> null params) (map defFunDecompose (filter (isDefFunOfSort (const True)) es))

-- instsBySort: mapping of sorts to instances
-- n: position of parameter (starting with 0)
-- pn: parameter name
-- ps: parameter sort
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

argMapToArgList :: Argmap -> [String]
argMapToArgList = map (\(paramName, Atom argName) -> argName)


constructRelModel :: SExpr -> [(String, [String], SExpr)]
constructRelModel smtmd =
    let instances = getInstances (fromModel smtmd)
        sorts = nub (map snd instances)
        instsBySort = map (\s -> (s, map fst (filter (\(i, si) -> s == si) instances))) sorts
        preds = getPreds (fromModel smtmd)
        binds = map (bindingsOfPred instsBySort) preds
        -- mds: list of: function name, argument map, evaluated body
        relmod = concatMap (\(fn, argms, bd) -> map (\argm -> (fn, argMapToArgList argm, simpSmtExpr argm bd)) argms) binds
    in  relmod

instanceNameMap :: SExpr -> [(String, [String])]
instanceNameMap smtmd =
    let instanceNames = map fst (getInstances (fromModel smtmd))
        consts = Model.getConsts (fromModel smtmd)
        inm = map (\n -> (n, [cn | (cn, params, tp, Atom s) <- consts, s == n])) instanceNames
    in inm

-- name used for displaying an instance
instanceDisplayName :: [(String, [String])] -> String -> String
instanceDisplayName inm instName =
    case lookup instName inm of
        Just (assocName:nms) -> assocName
        _ -> instName

substName  :: (String -> String) -> SExpr -> SExpr
substName sb (Atom s) = Atom (sb s)
substName sb (List es) = List (map (substName sb) es)

printSExpr :: SExpr -> String
printSExpr (Atom s) = s
printSExpr (List es) = "(" ++ unwords (map printSExpr es) ++ ")"

displayFormatConst :: (String, [SExpr], String, SExpr) -> (String, String)
displayFormatConst (cn, params, tp, v) = (cn, printSExpr v)

-- for an SExpr representing an SMT model, the function returns:
-- - a list of synonymes (constants mapped to the same class instance)
-- - a list of names of boolean constants and their values (true / false)
-- - a list of names of integer constants and their values
-- - a lsit of triples: predicate name, argument list, value of predicate at this point (possibly complex expression) except for boolean constants
displayableModel :: SExpr -> ([[String]], [(String, String)], [(String, String)], [(String, [String], String)])
displayableModel smtmd =
    let allConsts = Model.getConsts (fromModel smtmd)
        boolConsts = [c | c@(cn, params, tp, v) <- allConsts, tp == "Bool"]
        intConsts = [c | c@(cn, params, tp, v) <- allConsts, tp == "Int"]
        inm = instanceNameMap smtmd
        idn = instanceDisplayName inm
        synonymes = [constNames | (instName, constNames) <- inm, length constNames > 1]
        relmod = constructRelModel smtmd
        relmodWOConstFalse = [ (reln, map idn args, printSExpr (substName idn e)) | (reln, args, e) <- relmod, not (null args), e /= smtFalse ]
    in (synonymes, map displayFormatConst boolConsts, map displayFormatConst intConsts, relmodWOConstFalse)

printDisplayableModel :: ([[String]], [(String, String)], [(String, String)], [(String, [String], String)]) -> String
printDisplayableModel (synonymes, boolConsts, intConsts, relmods) =
    let synonymesS = case synonymes of
                        []  -> ""
                        _ -> "Synonymes: \n" ++ unlines (map unwords synonymes)
        boolConstsS = case boolConsts of
                        []  -> ""
                        _ -> "Propositional variables: \n" ++ unlines (map (\c -> fst c ++ " = " ++ snd c) boolConsts)
        intConstsS = case intConsts of
                        []  -> ""
                        _ -> "Integer variables: \n" ++ unlines (map (\c -> fst c ++ " = " ++ snd c) intConsts)
        relmodsS = case relmods of
                        []  -> ""
                        _ -> "Predicates: \n" ++
                             unlines (map (\(pn, args, v) -> pn ++ " " ++ unwords args ++ " = " ++ v) relmods) ++
                             "Remaining predicates: false"
    in synonymesS ++ boolConstsS ++ intConstsS ++ relmodsS

