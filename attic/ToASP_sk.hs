module ToASP where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import L4.Syntax
import L4.KeyValueMap
    ( ValueKVM(IdVM) )
import L4.PrintProg (showL4, PrintCurried (MultiArg), PrintConfig (PrintVarCase, PrintCurried), PrintVarCase (CapitalizeLocalVar))
import RuleTransfo (ruleDisjL, clarify)
import Data.Maybe (fromJust, mapMaybe)
import L4.SyntaxManipulation (decomposeBinop, appToFunArgs, funArgsToApp, applyVars)
import Data.List (nub)
import Data.Foldable (find)
import L4.PrintProg (capitalise)

data ASPRule t = ASPRule {
                     nameOfASPRule :: String
                   , varDeclsOfASPRule :: [VarDecl t]
                   , precondOfASPRule :: [Expr t]
                   , postcondOfASPRule :: Expr t }
  deriving (Eq, Ord, Show, Read)


-- Skolemized ASP rules code



skolemizedASPRuleName r = nameOfASPRule r
skolemizedASPRulePostcond r = postcondOfASPRule r
skolemizedASPRulePrecond r = [transformPrecond precon (postcondOfASPRule r) (varDeclsOfASPRule r) (nameOfASPRule r) | precon <- (precondOfASPRule r)]
--skolemizedASPRuleVardecls r = genSkolemList (varDeclsOfASPRule r) ([varExprToDecl expr (varDeclsOfASPRule r) | expr <- snd (appToFunArgs [] (postcondOfASPRule r))]) (nameOfASPRule r)

skolemizedASPRuleVardecls :: Eq t => ASPRule t -> [VarDecl t]
skolemizedASPRuleVardecls r =
    genSkolemList (varDeclsOfASPRule r) (map (convertVarExprToDecl (varDeclsOfASPRule r)) (snd (appToFunArgs [] (postcondOfASPRule r)))) (nameOfASPRule r)

-- skolemizeASPRule :: ASPRule t -> ASPRule t

skolemizeASPRule :: ASPRule (Tp ()) -> ASPRule (Tp ())
skolemizeASPRule r = ASPRule (skolemizedASPRuleName r) (skolemizedASPRuleVardecls r) (skolemizedASPRulePrecond r) (skolemizedASPRulePostcond r)


findVarDecl :: VarName -> [VarDecl t2] -> VarDecl t2
findVarDecl varname decls = fromJust (find (\d -> varname == nameOfVarDecl d) decls)

convertVarExprToDecl :: [VarDecl t2] -> Expr t ->VarDecl t2
convertVarExprToDecl decls (VarE _ v) = findVarDecl (nameOfQVarName (nameOfVar v)) decls
convertVarExprToDecl _decls _ = error "trying to convert a non-variable expression to a declaration"

--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
transformPrecond :: Expr (Tp ()) -> Expr (Tp ()) -> [VarDecl (Tp ())] -> [Char] -> Expr (Tp ())
transformPrecond precon postcon vardecls ruleid =
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
                let preconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon))
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
                    postconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] postcon))
                    new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec ruleid
                    new_precond = funArgsToApp (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)
                in new_precond


--genSkolem ::  VarDecl t -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls and returns skolemized precon var_decl
genSkolem :: Eq t => VarDecl t -> [VarDecl t] -> [Char] -> VarDecl t
genSkolem (VarDecl t vn u) y z = if elem (VarDecl t vn u) y
      then (VarDecl t (capitalise vn) u)
      else (VarDecl t ("skolemFn" ++ "_" ++ z ++ "_" ++ (capitalise vn) ++ (toBrackets y)) u)

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [Char] -> [VarDecl t]
genSkolemList x y z = [genSkolem xs y z | xs <- x]

toBrackets :: [VarDecl t] -> String
toBrackets [] = "()"
toBrackets [VarDecl t vn u] = "(" ++ capitalise vn ++ ")"
toBrackets ((VarDecl t vn u):xs) = "(" ++ capitalise vn ++ "," ++ tail (toBrackets xs)

-- var to var expr
mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}
--Takes in a var_decl and turns it into a var_expr
varDeclToExpr (VarDecl x y z) = mkVarE (GlobalVar (QVarName x y))


varDeclToExprTup (VarDecl x y z) = (varDeclToExpr (VarDecl x y z),(VarDecl x y z))

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

ruleToASPRule :: Rule (Tp ()) -> (ASPRule (Tp ()), [(Var (Tp ()), Var (Tp ()), Int)])
ruleToASPRule r =
    let precondsNeg = map negationPredicate (decomposeBinop (BBool BBand)(precondOfRule r))
        postcondNeg = negationPredicate (postcondOfRule r)
        preconds = map fst precondsNeg
        postcond = fst postcondNeg
        negpreds = mapMaybe snd (postcondNeg : precondsNeg)
    in  ( ASPRule
                (fromJust $ nameOfRule r)
                (varDeclsOfRule r)
                preconds
                postcond
        , negpreds)


data TranslationMode = AccordingToR | CausedByR | ExplainsR | VarSubs1R | VarSubs2R | VarSubs3R | AccordingToE String | LegallyHoldsE | RawL4
class ShowASP x where
    showASP :: TranslationMode -> x -> Doc ann
class ShowOppClause x where
    showOppClause :: x -> Doc ann

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => ShowASP (Expr t) where
    showASP (AccordingToE rn) e =
        pretty "according_to" <> parens (pretty rn <> pretty "," <+> showASP RawL4 e)

    -- predicates (App expressions) are written wrapped into legally_holds,
    -- whereas any other expressions are written as is.
    showASP LegallyHoldsE e@AppE{} =
        pretty "legally_holds" <> parens (showASP RawL4 e)
    showASP LegallyHoldsE e =
        showASP RawL4 e

    showASP RawL4 e = showL4 aspPrintConfig e
    showASP _ _ = pretty ""   -- not implemented

instance Show t => ShowOppClause (OpposesClause t) where
    showOppClause (OpposesClause pos neg) =
        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>
        pretty ":-" <+>
            showASP (AccordingToE "R") pos <> pretty "." <> line <>
        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>
        pretty ":-" <+>
            showASP (AccordingToE "R") neg <> pretty "." <> line <>

        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>


        pretty ":-" <+>
            showASP LegallyHoldsE pos <> pretty "." <> line <>

        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>

        pretty ":-" <+>
            showASP LegallyHoldsE neg <> pretty "." <> line <>

        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>

        pretty ":-" <+>
        pretty "query" <>
        parens (
                showASP RawL4 pos <+>
                pretty "," <>
                pretty "_N"
                ) <> pretty "." <> line <>




        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>

        pretty ":-" <+>
        pretty "query" <>
        parens (
                showASP RawL4 neg <+>
                pretty "," <>
                pretty "_N"
                ) <> pretty "."





instance Show t => ShowASP (ASPRule t) where
    showASP AccordingToR (ASPRule rn _vds preconds postcond) =
        showASP (AccordingToE rn) postcond <+> pretty ":-" <+>
            hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "."

{-     showASP ExplainsSkolemR (ASPRule rn vds preconds postcond)=
                             let new_rn = rn
                                 new_vds = skolemizedASPRuleVardecls (ASPRule rn vds preconds postcond)
                                 new_preconds = skolemizedASPRulePrecond (ASPRule rn vds preconds postcond)
                                 new_precond = postcond
                             in showASP ExplainsR (ASPRule rn new_vds new_preconds postcond)
 -}

    showASP ExplainsR (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "explains" <>
                    parens (
                        showASP RawL4 pc <> pretty "," <+>
                        showASP RawL4 postcond <+>
                        pretty "," <>
                        pretty "_N+1"
                        ) <+>
                    pretty ":-" <+>
                    pretty "query" <>
                    parens (
                            showASP RawL4 postcond <+>
                            pretty "," <>
                            pretty "_N"
                            ) <>
                    pretty "."
                    )
            preconds)










    showASP VarSubs3R (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ skolemize2 _vds postcond _rn ++ "," ++ "_N+1" ++ ")") <+>
                    pretty ":-" <+>
                    pretty "query" <>
                    parens (
                            showASP RawL4 postcond <+>
                            pretty "," <>
                            pretty "_N"
                            ) <>
                    pretty ", _N < M-1, max_ab_lvl(M)" <>
                    pretty "."
                    )
            [head preconds])

    showASP VarSubs1R (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "explains" <>
                    parens (
                        showASP RawL4 pc <> pretty "," <+>
                        showASP RawL4 postcond <+>
                        pretty "," <>
                        pretty "_N"
                        ) <+>
                    pretty ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets _vds ++ "," ++ "_N" ++ ").")
                    )
            preconds)


    showASP VarSubs2R (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc _vds) (varDeclToVarStrList _vds)) ++ "," ++ "M-1" ++ ")")
                         <+>
                    pretty ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList _vds)) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                    showASP LegallyHoldsE pc <>
                    pretty ", _N < M, max_ab_lvl(M)" <>
                    pretty "."
                    )
            preconds)
    showASP CausedByR (ASPRule rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "caused_by" <>
                        parens (
                            pretty "pos," <+>
                            showASP LegallyHoldsE pc <> pretty "," <+>
                            showASP (AccordingToE rn) postcond <> pretty "," <+>
                            pretty "_N+1"
                            ) <+>
                        pretty ":-" <+>
                        showASP (AccordingToE rn) postcond <> pretty "," <+>
                        hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "," <+>
                        pretty "justify" <>
                        parens (
                            showASP (AccordingToE rn) postcond <>  pretty "," <+>
                            pretty "_N") <>
                        pretty "."
                    )
            preconds)
    showASP _ _ = pretty ""  -- not implemented

genOppClause :: (Var (Tp ()), Var (Tp ()), Int) -> OpposesClause (Tp ())
genOppClause (posvar, negvar, n) =
    let args = zipWith (\ vn i -> LocalVar (QVarName IntegerT (vn ++ show i)) i) (replicate n "V") [0 .. n-1]
    in OpposesClause (applyVars posvar args) (applyVars negvar args)

astToASP :: Program (Tp ()) -> IO ()
astToASP prg = do
    let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
    -- putStrLn "Simplified L4 rules:"
    -- putDoc $ vsep (map (showL4 []) rules) <> line
    let aspRulesWithNegs = map ruleToASPRule rules
    let aspRules = map fst aspRulesWithNegs
    let skolemizedASPRules = map skolemizeASPRule aspRules
    let oppClausePrednames = nub (concatMap snd aspRulesWithNegs)
    let oppClauses = map genOppClause oppClausePrednames
    -- putStrLn "ASP rules:"
    putDoc $ vsep (map (showASP AccordingToR) aspRules) <> line <> line
    putDoc $ vsep (map (showASP VarSubs1R) aspRules) <> line <> line
    putDoc $ vsep (map (showASP VarSubs3R) aspRules) <> line <> line
    putDoc $ vsep (map (showASP VarSubs2R) aspRules) <> line <> line
    -- putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line <> line
    -- putDoc $ vsep (map (showASP ExplainsR) skolemizedASPRules) <> line <> line
    putDoc $ vsep (map (showASP CausedByR) aspRules) <> line <> line
    putDoc $ vsep (map showOppClause oppClauses) <> line


-- TODO: details to be filled in
proveAssertionASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
proveAssertionASP p v asrt = putStrLn "ASP solver implemented"


-- Additional functions to write var substitution code

preconToVarStrList :: Expr t ->[VarDecl t] -> [String]
preconToVarStrList precon vardecls = varDeclToVarStrList(map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon)))

varDeclToVarStrList :: [VarDecl t] -> [String]

varDeclToVarStrList [] = []
varDeclToVarStrList ((VarDecl t vn u) : xs) = ((capitalise vn) : varDeclToVarStrList xs)

my_str_trans :: [String] -> String -> String
my_str_trans s t = if elem t s
      then t
else "V" ++ "_" ++ t

my_str_trans_list s ts = [my_str_trans s t | t <- ts]



my_str_trans2 :: String -> [String] -> String -> String
my_str_trans2 v postc rulen  = if elem v postc
      then v
else "skolemFn" ++ "_" ++ rulen ++ "_" ++ v ++ toBrackets2 postc

my_str_trans_list2 s t u = [my_str_trans2 r t u | r <- s]



toBrackets2 :: [String] -> String

toBrackets2 [] = "()"
toBrackets2 [x] = "(" ++ x ++ ")"
toBrackets2 (x:xs) = "(" ++ x ++ "," ++ tail (toBrackets2 xs)


toBrackets3 :: [VarDecl t] -> String
toBrackets3 [] = "()"
toBrackets3 [VarDecl t vn u] = "(" ++ vn ++ ")"
toBrackets3 ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)


skolemize2 vardecs postc rulename =  toBrackets2 (my_str_trans_list2 (varDeclToVarStrList (vardecs)) (varDeclToVarStrList ((map (convertVarExprToDecl vardecs) (snd (appToFunArgs [] postc))))) rulename)
