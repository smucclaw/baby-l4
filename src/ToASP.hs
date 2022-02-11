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
skolemizedASPRuleVardecls r = genSkolemList (varDeclsOfASPRule r) ([varExprToDecl expr (varDeclsOfASPRule r) | expr <- snd (appToFunArgs [] (postcondOfASPRule r))]) (nameOfASPRule r)

-- skolemizeASPRule :: ASPRule t -> ASPRule t

skolemizeASPRule r = ASPRule (skolemizedASPRuleName r) (skolemizedASPRuleVardecls r) (skolemizedASPRulePrecond r) (skolemizedASPRulePostcond r)



--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
transformPrecond precon postcon vardecls ruleid =
                let preconvar_dec = [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
                    postconvar_dec = [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
                    new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec ruleid
                    new_precond = funArgsToApp (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)
                in new_precond


--genSkolem ::  VarDecl t -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls and returns skolemized precon var_decl
genSkolem (VarDecl t vn u) y z = if elem (VarDecl t vn u) y
      then (VarDecl t vn u)
      else (VarDecl t ("skolemFn" ++ "_" ++ z ++ "_" ++ vn ++ (toBrackets y)) u)

-- List version of genSkolem
genSkolemList x y z = [genSkolem xs y z | xs <- x]

toBrackets :: [VarDecl t] -> String
toBrackets [] = "()"
toBrackets [VarDecl t vn u] = "(" ++ vn ++ ")"
toBrackets ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)

-- var to var expr
mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}
--Takes in a var_decl and turns it into a var_expr
varDeclToExpr (VarDecl x y z) = mkVarE (GlobalVar (QVarName x y))


varDeclToExprTup (VarDecl x y z) = (varDeclToExpr (VarDecl x y z),(VarDecl x y z))

-- Matching function

match x (y:ys) = if x == fst y
     then snd y
     else match x ys

-- var expr to var decl, takes in a var expr and returns its corresponding var decl
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


data TranslationMode = AccordingToR | CausedByR | ExplainsR | AccordingToE String| ExplainsSkolemR | LegallyHoldsE | RawL4
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

    showASP ExplainsSkolemR (ASPRule _rn _vds preconds postcond)=
                             let new_rn = _rn
                                 new_vds = skolemizedASPRuleVardecls (ASPRule _rn _vds preconds postcond)
                                 new_preconds = skolemizedASPRulePrecond (ASPRule _rn _vds preconds postcond)
                                 new_precond = postcond
                             in showASP ExplainsR (ASPRule _rn new_vds new_preconds postcond)



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
    let oppClausePrednames = nub (concatMap snd aspRulesWithNegs)
    let oppClauses = map genOppClause oppClausePrednames
    -- putStrLn "ASP rules:"
    putDoc $ vsep (map (showASP AccordingToR) aspRules) <> line <> line
    putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line <> line
    putDoc $ vsep (map (showASP ExplainsSkolemR) aspRules) <> line <> line
    putDoc $ vsep (map (showASP CausedByR) aspRules) <> line <> line
    putDoc $ vsep (map showOppClause oppClauses) <> line


-- TODO: details to be filled in
proveAssertionASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
proveAssertionASP p v asrt = putStrLn "ASP solver implemented"
