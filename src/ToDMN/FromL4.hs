{-# LANGUAGE OverloadedStrings #-}

module ToDMN.FromL4 where

-- import Debug.Trace
import qualified Data.List as List
import qualified Data.Function as Fn
import Control.Arrow ( Arrow((&&&)) )
import ToDMN.Types
-- import ToRules.ToDecls (filterDecls, varDeclToProductionClassDecl, varDefnToProductionDefn)
-- import ToRules.ToRules (filterRule)
import L4.Syntax
import L4.SyntaxManipulation
-- import Data.Either (rights)
import L4.Typing
import L4.PrintProg

import Text.Pretty.Simple ( pPrint )

import ToDMN.FromSimpleToReg

import Control.Monad.Trans.State (runState)
import qualified Data.Map as Map

obtRule :: Program (Tp ()) -> String -> [Rule (Tp ())]
obtRule prog rname = [r | r <- rulesOfProgram prog, nameOfRule r == Just rname ]

-- mkProd :: Program (Tp ()) -> ProductionSystem
-- mkProd x = ProductionSystem {
--       boilerplate = ""
--     , functions = map varDefnToProductionDefn gdefns
--     , queries = ""
--     , classDecls = map (uncurry varDeclToProductionClassDecl) gdecls
--     , globals = ""
--     , rules = grules
--     } where gdecls = rights $ map filterDecls $ varDeclsOfProgram x
--             gdefns = varDefnsOfProgram x
--             grules = rights $ map filterRule $ rulesOfProgram x

genDMN :: Program (Tp ()) -> IO ()
genDMN x = do
    -- let rf = Drools
    -- let rf = Clara
    -- print $ showForm rf $ mkProd x

    let rs = rulesOfProgram x

    let filtered = filterRules rs


    -- let extractName = (map . map) nameOfRule
    -- let unsortedGroup = extractName $ classifyHeadPred filtered
    -- let sortedGroup = extractName $ (List.groupBy ((==) `Fn.on` headPredOf) . List.sortOn headPredOf) filtered

    let classDecls = classDeclsOfProgram x
    let varDecls = varDeclsOfProgram x
    let env = initialEnvOfProgram classDecls varDecls
    let allPreds = globalsOfEnv env
    let dec = decomposeBinop (BBool BBand)

    let groupedRules = groupBy' headPredOf filtered
    -- e.g. [(I, [r6, facti]), ...]

    let predMap = ruleGpsToPredGps groupedRules

    let allTables = allRulesToTables allPreds (groupBy' headPredOf filtered)

    -- pPrint "isValidPrecond"
    -- pPrint $ map isValidPrecond rs
    -- putStrLn "\n"

    -- pPrint "isValidPostcond"
    -- pPrint $ map isValidPostcond rs
    -- putStrLn "\n"

    -- pPrint $ map nameOfRule $ filterRules rs
    -- pPrint "filtered rules"
    -- pPrint filtered


    -- print ( classifyBy (\a b -> (a `mod` 3) == (b `mod` 3)) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
    -- pPrint $ classifyHeadPred $ filterRules rs

    -- ((+) `on` f) x y = f x + f y
    -- pPrint $ (compare `Fn.on` headPredOf) (head filtered) (last filtered)

    -- pPrint "rules grouped by sorted head preds"
    -- pPrint groupedRules
    -- putStrLn "\n"

    -- pPrint "all preds"
    -- pPrint allPreds
    -- putStrLn "\n"

    -- pPrint "decompose"
    -- pPrint $ concatMap (dec . precondOfRule) (snd $ head groupedRules)
    -- pPrint $ filter isTrueV $ concatMap (dec . precondOfRule) (snd $ head groupedRules)

    -- pPrint "pred map"
    -- pPrint predMap
    -- putStrLn "\n"

    -- pPrint "all preds"
    -- pPrint allPreds
    -- putStrLn "\n"

    -- pPrint "schema"
    -- pPrint $ map (schematize allPreds) predMap
    -- putStrLn "\n"


    -- pPrint "classDecls"
    -- pPrint classDecls

    -- pPrint "varDecls"
    -- pPrint varDecls

    -- pPrint "globals of env"
    -- pPrint allPreds

    -- pPrint "1st elem of env globals"
    -- pPrint $ head allPreds

    -- pPrint "pred type of P1"
    -- pPrint $ lookupPredType "P1" allPreds


    -- pPrint "input entries for r1"
    -- pPrint $ mkInputEntries ["P1", "P2"] $ head rs
    -- putStrLn "\n"

    -- pPrint "dmn rule for r1"
    -- pPrint $ mkRuleLine $ head rs

    -- pPrint "table I"
    -- pPrint $ ruleGroupToTable allPreds (head $ groupBy' headPredOf filtered)
    -- putStrLn "\n"

    -- pPrint "all tables"
    -- pPrint allTables

    let (decisions, _idState) = runState (mapM sDecisionToDecision allTables) Map.empty
    pPrint decisions

isValE :: Expr t -> Bool
isValE ValE {} = True
isValE _       = False

isVarE :: Expr t -> Bool
isVarE VarE {} = True
isVarE _  = False

-- does AppE have only 1 arg?
-- is each arg in AppE a value?
-- appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])
unaryApp :: Expr t -> Bool
unaryApp appE =
  let (f, es) = appToFunArgs [] appE
  in isVarE f && length es == 1 && all isValE es


-- is precond a conj of unary function applications?
isValidPrecond :: Rule t -> Bool
isValidPrecond = conjHasSimplePreds . precondOfRule

-- is each pred in conjE a unary function application?
-- decomposeBinop :: BinOp -> Expr t -> [Expr t]
conjHasSimplePreds :: Expr t -> Bool
conjHasSimplePreds bop = all unaryApp (decomposeBinopClean (BBool BBand) bop)


-- is postcond a unary function application?
isValidPostcond :: Rule t -> Bool
isValidPostcond = unaryApp . postcondOfRule


-- filter for well-formed rules
-- preconds should be conjs of app of single pred to single arg
-- postcond should app of single pred to single arg
filterRules :: [Rule t] -> [Rule t]
filterRules = filter (\r -> isValidPrecond r && isValidPostcond r)


-- classify :: Eq a => [a] -> [[a]]
-- classify = classifyBy (==)

-- classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- classifyBy eq = List.foldl' f [ ]
--   where
--     f [ ] y = [[y]]
--     f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
--                           | otherwise = xs: f xss y

classifyHeadPred :: [Rule t] -> [[Rule t]]
-- classifyHeadPred = classifyBy hasSameHeadPred
classifyHeadPred = List.groupBy hasSameHeadPred

hasSameHeadPred :: Rule t -> Rule t -> Bool
hasSameHeadPred rx ry = headPredOf rx == headPredOf ry

-- group rules that have same output preds together
-- should also include facts
-- decompose postcond into fn and arg and extract fn name
headPredOf :: Rule t -> String
headPredOf r =
  let (f, _) = appToFunArgs [] (postcondOfRule r)
  in getNameOfPred f

groupBy' :: (Rule t -> String) -> [Rule t] ->[(String, [Rule t])]
groupBy' f = map (f . head &&& id)
             . List.groupBy ((==) `Fn.on` f)
             . List.sortOn f -- sortBy (compare `on` f)

-- sortOn :: (Rule t -> String) -> [Rule t] -> [Rule t]
-- groupBy :: (Rule t -> Rule t -> Bool) ->[Rule t] -> [[Rule t]]

-- getNameOfVal :: Expr t -> String
-- getnameOfVal = printVal . valOfExprValE

getNameOfPred :: Expr t -> String
getNameOfPred = nameOfQVarName . nameOfVar . varOfExprVarE

-- does different things depending on whether pred is a bool or funApp
-- mkPredName :: Expr t -> String
-- mkPredName (ValE _ (BoolV True)) = ""
-- mkPredName fa@AppE {} = (getNameOfPred . fst . appToFunArgs []) fa
-- mkPredName _ = error "Precondition should be either a function application or True"


-- form a schema for each equivalence class
-- fns need better names
-- ("O", [r2, r3]) -> ("O", ["P1", "P3", "P2"])
-- schema list : ("O", ["P1", "P3", "P2"])
-- given an output pred and all the rules that produce it
-- generates a list of input preds from those rules
ruleGpToPredGp :: Show t => (String, [Rule t]) -> (String, [String])
ruleGpToPredGp (hp, rs) =
  let decomp = decomposeBinopClean (BBool BBand)
      debops = concatMap (decomp . precondOfRule) rs
      getPN = getNameOfPred . fst . appToFunArgs []
  in (hp, List.nub (map getPN debops))

ruleGpsToPredGps :: Show t => [(String, [Rule t])] -> [(String, [String])]
ruleGpsToPredGps = map ruleGpToPredGp


-- pred types need to be extracted too
schematize ::  VarEnvironment -> (String, [String]) -> SimpleSchema
schematize env (hp, body) =
  SimpleSchema
  (map (mkSimpleInputSchema env) body)
  (mkSimpleOutputSchema env hp)

mkSimpleInputSchema :: VarEnvironment -> String -> SimpleInputSchema
mkSimpleInputSchema env "" = undefined
mkSimpleInputSchema env pd = SimpleInputSchema pd (stringTpToFEELTp (lookupPredType pd env))

mkSimpleOutputSchema :: VarEnvironment -> String -> SimpleOutputSchema
mkSimpleOutputSchema env pd = SimpleOutputSchema pd (stringTpToFEELTp (lookupPredType pd env))

-- TODO
-- how to deal with predicates of arbitrary classes (invalid types)
stringTpToFEELTp :: String -> FEELType
stringTpToFEELTp "String" = String
stringTpToFEELTp "Boolean" = Bool -- is this actually "Boolean"?
stringTpToFEELTp "Integer" = Number
stringTpToFEELTp _ = error "not a valid FEELType"


-- given pred name, looks up its type
-- varenv is [(predname, FunT)]
-- lookup should always succeed
lookupPredType :: String -> VarEnvironment -> String
lookupPredType nm env =
  let (Just funt) = lookup nm env
  in (stringOfClassName . classNameOfTp . paramTp) funt


-- input is a single element of this
-- [
--     ( "O"
--     ,
--         [ "P1"
--         , "P3"
--         , "P2"
--         ]
--     )
-- ,
--     ( "O2"
--     ,
--         [ "P1"
--         , "P2"
--         ]
--     )
-- ]

-- table O
-- P1 | P3 | P2    | O
-- 2  | 4  | False | 10
-- 1  | -  | -     | 11

-- TODO
-- we do not deal with dependent tables yet

-- [("O", [r2, r3]), ("O2", [r1])]
allRulesToTables :: Show t => VarEnvironment -> [(String, [Rule t])] -> [SimpleDecision]
allRulesToTables env = map (ruleGroupToTable env)

-- given a varenv, output pred and corresponding rules e.g. ("O", [r2, r3])
-- generates a simple decision table
ruleGroupToTable :: Show t => VarEnvironment -> (String, [Rule t]) -> SimpleDecision
ruleGroupToTable env rg@(_, rs) =
  let predGp = ruleGpToPredGp rg
      schema = schematize env predGp
      ruleLines = map (mkRuleLine predGp) rs
      inputPreds = snd predGp
  in SimpleDecTableEl (map SimpleReqInputEl inputPreds) schema ruleLines

-- given a list of input preds and a rule, generates a list of input entries
-- schemaPreds (from O): ["P1", "P3", "P2"]
-- funArgs: [(P1, [11]), (P2, [True]), (P3, [33])]
-- delistedFunArgs : [(P1, 11), (P2, True), (P3, 33)]
mkInputEntries :: Show t => [String] -> Rule t -> [SimpleInputEntry]
mkInputEntries schemaInpPreds r =
  let funApps = decomposeBinopClean (BBool BBand) (precondOfRule r)
      funArgs = map (appToFunArgs []) funApps
      delistedFunArgs = map (\(fn, args) -> (getNameOfPred fn, head args)) funArgs
  in map (foo delistedFunArgs) schemaInpPreds

-- looks up schema pred against map of rule pred-vals
foo :: Show t => [(String, Expr t)] -> String -> SimpleInputEntry
foo delistedFunArgs pd =
  case lookup pd delistedFunArgs of
    Just val -> SimpleInputEntry (Just (XMLText (printExpr val)))
    Nothing -> SimpleInputEntry Nothing

-- given an output pred and a rule, generates an output entry
mkOutputEntry :: Show t => String -> Rule t -> SimpleOutputEntry
mkOutputEntry _ r =
  let funApp = postcondOfRule r
      (fn, args) = appToFunArgs [] funApp
      -- delistedFunArgs = (getNameOfPred fn, head args)
  in bar (head args)

bar :: Show t => Expr t -> SimpleOutputEntry
bar val = SimpleOutputEntry (XMLText (printExpr val))

-- given a schema ("O2", ["P1", "P2"]) and a rule
mkRuleLine :: Show t => (String, [String]) -> Rule t -> SimpleDMNRule
mkRuleLine (_, inPreds) r =
  let
    bopToApps = decomposeBinopClean (BBool BBand)
    appToPreds = getNameOfPred . fst . appToFunArgs []

    outFunApps = bopToApps (postcondOfRule r)
    -- outpred is always a singleton
    outPred = head $ map appToPreds outFunApps

  in SimpleDMNRule (mkInputEntries inPreds r) (mkOutputEntry outPred r)


-- decomposeBinop :: BinOp -> Expr t -> [Expr t]
-- appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])


-- assume that all variables come from tables

