 {-# LANGUAGE OverloadedStrings #-}

module ToEpilog where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
--import ansi-wl-pprint
import L4.Syntax
import L4.PrintProg
    ( showL4,
      PrintCurried(MultiArg),
      PrintConfig(PrintVarCase, PrintCurried),
      PrintVarCase(CapitalizeLocalVar),
      capitalise )
import RuleTransfo (ruleDisjL, clarify)
import Data.Maybe (fromJust, mapMaybe)
import L4.SyntaxManipulation (decomposeBinop, appToFunArgs, funArgsToApp, applyVars, globalVarsOfProgram)
import Data.List (nub)
import Data.Foldable (find)
import L4.KeyValueMap (ValueKVM)


data ASPRule t = ASPRule {
                     nameOfASPRule :: String
                   , globalVarDeclsOfASPRule :: [VarDecl t]
                   , localVarDeclsOfASPRule :: [VarDecl t]
                   , precondOfASPRule :: [Expr t]
                   , postcondOfASPRule :: Expr t }
  deriving (Eq, Ord, Show, Read)


-- Skolemized ASP rules code


skolemizeASPRuleGlobals :: ASPRule t -> [VarDecl t]
skolemizeASPRuleGlobals = globalVarDeclsOfASPRule
skolemizedASPRuleName :: ASPRule t -> String
skolemizedASPRuleName = nameOfASPRule
skolemizedASPRulePostcond :: ASPRule t -> Expr t
skolemizedASPRulePostcond = postcondOfASPRule
skolemizedASPRulePrecond :: ASPRule (Tp ()) -> [Expr (Tp ())]
skolemizedASPRulePrecond r = [transformPrecond precon (postcondOfASPRule r) (localVarDeclsOfASPRule r ++ globalVarDeclsOfASPRule r) (globalVarDeclsOfASPRule r) (nameOfASPRule r) | precon <- precondOfASPRule r]
--skolemizedASPRuleVardecls r = genSkolemList (localVarDeclsOfASPRule r) ([varExprToDecl expr (localVarDeclsOfASPRule r) | expr <- snd (appToFunArgs [] (postcondOfASPRule r))]) (nameOfASPRule r)

skolemizedASPRuleVardecls :: Eq t => ASPRule t -> [VarDecl t]
skolemizedASPRuleVardecls r =
  genSkolemList (localVarDeclsOfASPRule r) (map (convertVarExprToDecl (localVarDeclsOfASPRule r)) (snd (appToFunArgs [] (postcondOfASPRule r)))) (globalVarDeclsOfASPRule r) (nameOfASPRule r)

-- skolemizeASPRule :: ASPRule t -> ASPRule t

skolemizeASPRule :: ASPRule (Tp ()) -> ASPRule (Tp ())
skolemizeASPRule r = ASPRule (skolemizedASPRuleName r)  (skolemizeASPRuleGlobals r) (skolemizedASPRuleVardecls r) (skolemizedASPRulePrecond r) (skolemizedASPRulePostcond r)


findVarDecl :: VarName -> [VarDecl t2] -> VarDecl t2
findVarDecl varname decls = fromJust (find (\d -> varname == nameOfVarDecl d) decls)

convertVarExprToDecl :: [VarDecl t2] -> Expr t ->VarDecl t2
convertVarExprToDecl decls (VarE _ v) = findVarDecl (nameOfQVarName (nameOfVar v)) decls
convertVarExprToDecl _decls _ = error "trying to convert a non-variable expression to a declaration"

--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
-- Need to change this !!! First : Check if precon occurs among vardecls, then check if postcon occurs among vardecls

transformPrecond :: Expr (Tp ()) -> Expr (Tp ()) -> [VarDecl (Tp ())] -> [VarDecl (Tp ())] -> [Char] -> Expr (Tp ())
transformPrecond precon postcon vardecls vardeclsGlobal ruleid =
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
                let preconvar_dec = map (convertVarExprToDecl (vardecls)) (snd (appToFunArgs [] precon))
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
                    postconvar_dec = map (convertVarExprToDecl (vardecls)) (snd (appToFunArgs [] postcon))
                    new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec vardeclsGlobal ruleid
                    new_precond = funArgsToApp (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)

                in new_precond


--genSkolem ::  VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls, list of global varDecls and returns skolemized precon var_decl
genSkolem :: Eq t => VarDecl t -> [VarDecl t] -> [VarDecl t] -> [Char] -> VarDecl t
genSkolem (VarDecl t vn u) y w z =
    if VarDecl t vn u `elem` w
       then VarDecl t  vn u
    else if VarDecl t vn u `elem` y
       then VarDecl t (capitalise vn) u
    else VarDecl t "extVar" u

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [VarDecl t] -> [Char] -> [VarDecl t]
genSkolemList x y w z = [genSkolem xs y w z | xs <- x]

toBrackets :: [VarDecl t] -> String
toBrackets [] = "()"
toBrackets [VarDecl _t vn _u] = "(" ++ capitalise vn ++ ")"
toBrackets ((VarDecl _t vn _u):xs) = "(" ++ capitalise vn ++ "," ++ tail (toBrackets xs)

-- var to var expr
mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}
--Takes in a var_decl and turns it into a var_expr
varDeclToExpr :: VarDecl t -> Expr t
varDeclToExpr (VarDecl x y _z) = mkVarE (GlobalVar (QVarName x y))


varDeclToExprTup :: VarDecl t -> (Expr t, VarDecl t)
varDeclToExprTup (VarDecl x y z) = (varDeclToExpr (VarDecl x y z), VarDecl x y z)

-- Matching function

match :: (Eq t, Show t) => t -> [(t, p)] -> p
match x (y:ys) = if x == fst y
     then snd y
     else match x ys
match x [] = error ("looking for " ++ show x)

-- var expr to var decl, takes in a var expr and returns its corresponding var decl
varExprToDecl :: (Eq t, Show t) => Expr t -> [VarDecl t] -> VarDecl t
varExprToDecl expr listdec = match expr (map varDeclToExprTup listdec)


-- Expression data structure in Syntax.hs
-- many additional functions in SyntaxManipuation.hs

-- End of Skoemized ASP rules code




data OpposesClause t = OpposesClause {
      posLit :: Expr t
    , negLit :: Expr t}
  deriving (Eq, Ord, Show, Read)


negationVarname :: QVarName t -> QVarName t
negationVarname (QVarName t vn) = QVarName t ("not"++vn)


negationPredicate :: Expr (Tp ()) -> (Expr (Tp ()), Maybe (Var (Tp ()), Var (Tp ()), Int))
negationPredicate (UnaOpE _ (UBool UBnot) e@AppE{}) =
    let (f, args) = appToFunArgs [] e in
        case f of
            VarE t posvar@(GlobalVar vn) ->
                let negvar = GlobalVar (negationVarname vn)
                in (funArgsToApp (VarE t negvar) args, Just (posvar, negvar, length args))
            _ -> error "negationPredicate: ill-formed negation"
negationPredicate e = (e, Nothing)

ruleToASPRule :: [VarDecl (Tp ())] -> Rule (Tp ()) -> (ASPRule (Tp ()), [(Var (Tp ()), Var (Tp ()), Int)])
ruleToASPRule globals r =
    let precondsNeg = map negationPredicate (decomposeBinop (BBool BBand)(precondOfRule r))
        postcondNeg = negationPredicate (postcondOfRule r)
        preconds = map fst precondsNeg
        postcond = fst postcondNeg
        negpreds = mapMaybe snd (postcondNeg : precondsNeg)
    in  ( ASPRule
                (fromJust $ nameOfRule r)
                globals
                (varDeclsOfRule r)
                preconds
                postcond
        , negpreds)


data TranslationMode = AccordingToR | CausedByR | ExplainsR | VarSubs1R | VarSubs2R | VarSubs3R | AccordingToE String | LegallyHoldsE | QueryE | VarSubs4R | RawL4 | AddFacts | FixedCode
class ShowASP x where
    showASP :: TranslationMode -> x -> Doc ann
class ShowOppClause x where
    showOppClause :: x -> Doc ann

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => ShowASP (Expr t) where
    showASP (AccordingToE rn) e =
        "according_to" <> parens (pretty rn <> "," <+> showASP RawL4 e)

    -- predicates (App expressions) are written wrapped into legally_holds,
    -- whereas any other expressions are written as is.
    showASP LegallyHoldsE e@AppE{} =
        "legally_holds" <> parens (showASP RawL4 e)

    showASP LegallyHoldsE e =
        "legally_holds" <> parens (showASP RawL4 e)
    showASP QueryE e@AppE{} =
        "query" <> parens (showASP RawL4 e <> "," <> "L")
    showASP QueryE e =
        "query" <> parens (showASP RawL4 e <> "," <> "L")

    -- showASP LegallyHoldsE e =
    --     showASP RawL4 e
    -- showASP QueryE e@AppE{} =
    --     "query" <> parens (showASP RawL4 e <> "," <> "L")
    -- showASP QueryE e =
    --     showASP RawL4 e

    showASP RawL4 e = showL4 aspPrintConfig e
    showASP _ _ = ""   -- not implemented

instance Show t => ShowOppClause (OpposesClause t) where
    showOppClause (OpposesClause pos neg) =
        "opposes" <>
            parens (showASP RawL4 pos <> "," <+> showASP RawL4 neg) <+>
        ":-" <+>
            showASP (AccordingToE "R") pos <> line <>
        "opposes" <>
            parens (showASP RawL4 pos <> "," <+> showASP RawL4 neg) <+>
        ":-" <+>
            showASP (AccordingToE "R") neg <> line <>

        "opposes" <>
            parens (showASP RawL4 pos <> "," <+> showASP RawL4 neg) <+>


        ":-" <+>
            showASP LegallyHoldsE pos <> line <>

        "opposes" <>
            parens (showASP RawL4 pos <> "," <+> showASP RawL4 neg) <+>

        ":-" <+>
            showASP LegallyHoldsE neg <> line <>

{-        "opposes" <>
            parens (showASP RawL4 pos <> "," <+> showASP RawL4 neg) <+>

        ":-" <+>
        "query" <>
        parens (
                showASP RawL4 pos <+>
                "," <>
                "_N"
                ) <> "." <> line <>



-}

       "opposes" <>
            parens (showASP RawL4 pos <> "," <+> showASP RawL4 neg) <+>

        ":-" <+>
            showASP LegallyHoldsE neg 
                
                





instance Show t => ShowASP (ASPRule t) where
    showASP AccordingToR (ASPRule rn _env _vds preconds postcond) =
        showASP (AccordingToE rn) postcond <+> ":-" <+>
            hsep (punctuate " &" (map (showASP LegallyHoldsE) preconds))

{-     showASP ExplainsSkolemR (ASPRule rn vds preconds postcond)=
                             let new_rn = rn
                                 new_vds = skolemizedASPRuleVardecls (ASPRule rn vds preconds postcond)
                                 new_preconds = skolemizedASPRulePrecond (ASPRule rn vds preconds postcond)
                                 new_precond = postcond
                             in showASP ExplainsR (ASPRule rn new_vds new_preconds postcond)
 -}

    showASP ExplainsR (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    "explains" <>
                    parens (
                        showASP RawL4 pc <> "," <+>
                        showASP RawL4 postcond <+>
                        "," <>
                        "_N+1"
                        ) <+>
                    ":-" <+>
                    "query" <>
                    parens (
                            showASP RawL4 postcond <+>
                            "," <>
                            "_N"
                            ) <>
                    "_N < M, max_ab_lvl(M)" <>
                    "."
                    )
            preconds)



-- TODO: weird: var pc not used in map
    showASP VarSubs3R (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ skolemize2 (_vds ++ _env) _vds postcond _rn ++ "," ++ "_N+1" ++ ")") <+>
                    ":-" <+>
                    "query" <>
                    parens (
                            showASP RawL4 postcond <+>
                            "," <>
                            "_N"
                            ) <>
                    ", _N < M, max_ab_lvl(M)" <>
                    "."
                    )
            [head preconds])

    showASP VarSubs1R (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    "explains" <>
                    parens (
                        showASP RawL4 pc <> "," <+>
                        showASP RawL4 postcond <+>
                        "," <>
                        "_N"
                        ) <+>
                    ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets _vds ++ "," ++ "_N" ++ ").")
                    )
            preconds)
     
    showASP FixedCode (ASPRule _rn _env _vds preconds postcond) = vsep (["defeated(R2,C2):-overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2)", "opposes(C1,C2):-opposes(C2,C1)", "legally_enforces(R,C):-according_to(R,C) & ~defeated(R,C) ", "legally_holds(C):-legally_enforces(R,C)", "legally_holds(contradiction_entailed):-opposes(C1,C2) & legally_holds(C1) & legally_holds(C2)", 
     "caused_by(pos,overrides(R1,R2),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)", "caused_by(pos,according_to(R2,C2),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)",
     "caused_by(pos,legally_enforces(R1,C1),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)","caused_by(pos,opposes(C1,C2),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)", "caused_by(pos,according_to(R,C),legally_enforces(R,C),0):-legally_enforces(R,C) & according_to(R,C) & ~defeated(R,C) & justify(legally_enforces(R,C),0)", "caused_by(neg,defeated(R,C),legally_enforces(R,C),0):-legally_enforces(R,C) & according_to(R,C) & ~defeated(R,C) & justify(legally_enforces(R,C),0)", "caused_by(pos,legally_enforces(R,C),legally_holds(C),0):-legally_holds(C) & legally_enforces(R,C) & justify(legally_holds(C),0)","justify(X,0):-caused_by(pos,X,Y,0)", "directedEdge(Sgn,X,Y):-caused_by(Sgn,X,Y,0)", "justify(X,0):-gen_graph(X)"]) 



      


    showASP AddFacts (ASPRule _rn _env _vds _preconds postcond) =
        vsep (map (\pc ->
                    "user_input" <>
                    parens (
                        showASP RawL4 pc <> "," <+>
                        pretty _rn

                        )

                    )
            [postcond])





    showASP VarSubs2R (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")")
                         <+>
                    ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                    showASP LegallyHoldsE pc <> "."
                    )
            (postcond : preconds))


    showASP VarSubs4R (ASPRule rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")")
                         <+>
                    ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                    showASP QueryE pc <> "."
                    )
            (postcond : preconds))

    showASP CausedByR (ASPRule rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    "caused_by" <>
                        parens (
                            "pos," <+>
                            showASP LegallyHoldsE pc <> "," <+>
                            showASP (AccordingToE rn) postcond <> "," <+>
                            "0"
                            ) <+>
                        ":-" <> line <> indent 4 (
                        vsep [indent 2 $ showASP (AccordingToE rn) postcond
                             , "&" <+> hang 0 (vsep (punctuate " &" (map (showASP LegallyHoldsE) preconds)))
                             , "&" <+> "justify" <> parens ( showASP (AccordingToE rn) postcond <>  "," <+> "0")
                        ]
                        )

                    )
            preconds)
    showASP _ _ = ""  -- not implemented

genOppClause :: (Var (Tp ()), Var (Tp ()), Int) -> OpposesClause (Tp ())
genOppClause (posvar, negvar, n) =
    let args = zipWith (\ vn i -> LocalVar (QVarName IntegerT (vn ++ show i)) i) (replicate n "V") [0 .. n-1]
    in OpposesClause (applyVars posvar args) (applyVars negvar args)

astToEpilog :: Program (Tp ()) -> IO ()
astToEpilog prg = do
    let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
    -- putStrLn "Simplified L4 rules:"
    -- putDoc $ vsep (map (showL4 []) rules) <> line
    let aspRulesWithNegs = map (ruleToASPRule (globalVarsOfProgram prg)) rules
    let aspRules = map fst aspRulesWithNegs
    let aspRulesNoFact = removeFacts aspRules
    let aspRulesFact = keepFacts aspRules
    let skolemizedASPRules = map skolemizeASPRule aspRulesNoFact
    let oppClausePrednames = nub (concatMap snd aspRulesWithNegs)
    let oppClauses = map genOppClause oppClausePrednames

    -- putStrLn "ASP rules:"
    putDoc $ vsep (map (showASP AccordingToR) aspRulesNoFact) <> line <> line
    -- putDoc $ vsep (map (showASP VarSubs1R) aspRulesNoFact) <> line <> line
    putDoc $ vsep (map (showASP AddFacts) aspRulesFact) <> line <> line
    putDoc $ vsep (map (showASP FixedCode) ([head aspRulesFact])) <> line <> line

    -- putDoc $ vsep (map (showASP VarSubs3R) aspRulesNoFact) <> line <> line
    -- putDoc $ vsep (map (showASP VarSubs2R) aspRulesNoFact) <> line <> line
    -- putDoc $ vsep (map (showASP VarSubs4R) aspRulesNoFact) <> line <> line
    -- putDoc $ vsep (map (showASP VarSubs2R) aspRules) <> line <> line
    -- putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line <> line
    -- putDoc $ vsep (map (showASP ExplainsR) skolemizedASPRules) <> line <> line
    putDoc $ vsep (map (showASP CausedByR) aspRulesNoFact) <> line <> line
    putDoc $ vsep (map showOppClause oppClauses) <> line


-- TODO: details to be filled in
proveAssertionASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
proveAssertionASP p v asrt = putStrLn "ASP solver implemented"


-- Additional functions to write var substitution code

preconToVarStrList :: Expr t ->[VarDecl t] -> [String]
preconToVarStrList precon vardecls = varDeclToVarStrList(map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon)))

varDeclToVarStrList :: [VarDecl t] -> [String]

varDeclToVarStrList [] = []
varDeclToVarStrList ((VarDecl t vn u) : xs) = capitalise vn : varDeclToVarStrList xs

my_str_trans :: [String] -> String -> String
my_str_trans s t = if elem t s
      then t
else "V" ++ "_" ++ t

my_str_trans_list s ts = [my_str_trans s t | t <- ts]



my_str_trans2 :: String -> [String] -> String -> String
my_str_trans2 v postc rulen  = if elem v postc
      then v
else "extVar"

my_str_trans_list2 s t u = [my_str_trans2 r t u | r <- s]



toBrackets2 :: [String] -> String

toBrackets2 [] = "()"
toBrackets2 [x] = "(" ++ x ++ ")"
toBrackets2 (x:xs) = "(" ++ x ++ "," ++ tail (toBrackets2 xs)


toBrackets3 :: [VarDecl t] -> String
toBrackets3 [] = "()"
toBrackets3 [VarDecl t vn u] = "(" ++ vn ++ ")"
toBrackets3 ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)


--skolemize2 :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> Expr t2 -> String -> String
skolemize2 vardecs localvar postc rulename =  toBrackets2 (my_str_trans_list2 (varDeclToVarStrList localvar) (varDeclToVarStrList ((map (convertVarExprToDecl vardecs) (snd (appToFunArgs [] postc))))) rulename)

isFact :: Expr t -> Bool
isFact (ValE _ (BoolV True)) = True
isFact _ = False

removeFacts :: [ASPRule t] -> [ASPRule t]
removeFacts [] = []
removeFacts (r : rs)
      | isFact (head (precondOfASPRule r)) = removeFacts rs
      | otherwise = r : removeFacts rs


keepFacts :: [ASPRule t] -> [ASPRule t]
keepFacts [] =[]
keepFacts (r : rs)
      | isFact (head (precondOfASPRule r)) = r : keepFacts rs
      | otherwise = keepFacts rs

filt :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> [VarDecl t1]
filt [] _ = []
filt (x:xs) ys =
  if x `elem` ys
     then filt xs ys
  else (x: (filt xs ys))
