module ToASP where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Syntax
import PrintProg (showL4, PrintCurried (MultiArg), PrintConfig (PrintVarCase, PrintCurried), PrintVarCase (CapitalizeLocalVar))
import RuleTransfo (ruleDisjL, clarify)
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import SyntaxManipulation (decomposeBinop, appToFunArgs, funArgsToApp, applyVars)
import Data.List (nub)
import Data.Graph.Inductive.Graph (prettify)

data ASPRule t = ASPRule {
                     nameOfASPRule :: String
                   , varDeclsOfASPRule :: [VarDecl t]
                   , precondOfASPRule :: [Expr t]
                   , postcondOfASPRule :: Expr t }
  deriving (Eq, Ord, Show, Read)

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
    let precondNeg = map negationPredicate (decomposeBinop (BBool BBand)(precondOfRule r))
        preconds = map fst precondNeg
        negpreds = mapMaybe snd precondNeg
    in  ( ASPRule
                (fromJust $ nameOfRule r)
                (varDeclsOfRule r)
                preconds
                (postcondOfRule r)
        , negpreds)


data TranslationMode = AccordingToR | CausedByR | ExplainsR | AccordingToE String| LegallyHoldsE | RawL4
class ShowASP x where
    showASP :: TranslationMode -> x -> Doc ann
class ShowOppClause x where
    showOppClause :: x -> Doc ann

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => ShowASP (Expr t) where
    showASP (AccordingToE rn) e =
        pretty "according_to" <> parens (pretty rn <> pretty "," <+> showASP RawL4 e)
    showASP LegallyHoldsE e =
        pretty "legally_holds" <> parens (showASP RawL4 e)
    showASP RawL4 e = showL4 aspPrintConfig e
    showASP _ _ = pretty ""   -- not implemented

instance Show t => ShowOppClause (OpposesClause t) where
    showOppClause (OpposesClause pos neg) =
        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>
        pretty ":-" <+>
            showASP (AccordingToE "R1") pos <> pretty "," <+>
            showASP (AccordingToE "R2") neg <>
        pretty "." <> line <>
        pretty ":-" <+>
        showASP LegallyHoldsE pos <> pretty "," <+>
        showASP LegallyHoldsE neg <>
        pretty "." 

instance Show t => ShowASP (ASPRule t) where
    showASP AccordingToR (ASPRule rn _vds preconds postcond) =
        showASP (AccordingToE rn) postcond <+> pretty ":-" <+>
            hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "."

    showASP ExplainsR (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "explains" <>
                    parens (
                        showASP RawL4 pc <> pretty "," <+>
                        showASP RawL4 postcond <+>
                        pretty "_N+1"
                        ) <+>
                    pretty ":-" <+>
                    pretty "query" <>
                    parens (
                            showASP RawL4 postcond <+>
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
    putStrLn "Simplified L4 rules:"
    putDoc $ vsep (map (showL4 []) rules) <> line
    let aspRulesWithNegs = map ruleToASPRule rules
    let aspRules = map fst aspRulesWithNegs
    let oppClausePrednames = nub (concatMap snd aspRulesWithNegs)
    let oppClauses = map genOppClause oppClausePrednames
    putStrLn "ASP rules:"
    putDoc $ vsep (map (showASP AccordingToR) aspRules) <> line
    putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line
    putDoc $ vsep (map (showASP CausedByR) aspRules) <> line
    putDoc $ vsep (map showOppClause oppClauses) <> line
