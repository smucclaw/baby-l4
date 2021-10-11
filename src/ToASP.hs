module ToASP where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Syntax
import PrintProg (showL4, PrintCurried (MultiArg), PrintConfig (PrintVarCase, PrintCurried), PrintVarCase (CapitalizeLocalVar))
import RuleTransfo (ruleDisjL, clarify)
import Data.Maybe (fromJust)
import SyntaxManipulation (decomposeBinop)

data ASPRule t = ASPRule {
                     nameOfASPRule :: String
                   , varDeclsOfASPRule :: [VarDecl t]
                   , precondOfASPRule :: [Expr t]
                   , postcondOfASPRule :: Expr t }
  deriving (Eq, Ord, Show, Read)

ruleToASPRule :: Rule t -> ASPRule t
ruleToASPRule r = ASPRule
                        (fromJust $ nameOfRule r)
                        (varDeclsOfRule r)
                        (decomposeBinop (BBool BBand)(precondOfRule r))
                        (postcondOfRule r)


data TranslationMode = AccordingToR | CausedByR | ExplainsR | AccordingToE String| LegallyHoldsE | RawL4
class ShowASP x where
    showASP :: TranslationMode -> x -> Doc ann

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => ShowASP (Expr t) where
    showASP (AccordingToE rn) e =
        pretty "according_to" <> parens (pretty rn <> pretty "," <+> showASP RawL4 e)
    showASP LegallyHoldsE e =
        pretty "legally_holds" <> parens (showASP RawL4 e)
    showASP RawL4 e = showL4 aspPrintConfig e
    showASP _ _ = pretty ""   -- not implemented

instance Show t => ShowASP (ASPRule t) where
    showASP AccordingToR (ASPRule rn _vds preconds postcond) =
        showASP (AccordingToE rn) postcond <+> pretty ":-" <+>
            hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "."

    -- TODO: add the arguments N+1 (in explains) resp. N (in query)
    -- Is there syntax for internal variables such as _N to avoid having to generate unique names?
    showASP ExplainsR (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "explains" <> parens (showASP RawL4 pc <> pretty "," <+> showASP RawL4 postcond) <+> pretty ":-" <+>
                    pretty "query" <> parens(showASP RawL4 postcond) <> pretty "."
                    )
            preconds)

    -- TODO: there is a "pos" argument for caused_by that is not motivated and that occurs in the rule head but nowhere else
    -- TODO: add the arguments N+1 (in caused_by) resp. N (in justify)
    showASP CausedByR (ASPRule _rn _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "caused_by" <> parens (showASP LegallyHoldsE pc <> pretty "," <+> showASP (AccordingToE "???") postcond) <+> pretty ":-" <+>
                        showASP (AccordingToE "???") postcond <> pretty "," <+>
                        hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "," <+>
                        pretty "justify" <> parens (showASP (AccordingToE "???") postcond) <> pretty "."
                    )
            preconds)
    showASP _ _ = pretty ""  -- not implemented

astToASP :: Program (Tp ()) -> IO ()
astToASP prg = do
    let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
    putStrLn "Simplified L4 rules:"
    putDoc $ vsep (map (showL4 []) rules) <> line
    let aspRules = map ruleToASPRule rules
    putStrLn "ASP rules:"
    putDoc $ vsep (map (showASP AccordingToR) aspRules) <> line
    putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line
    putDoc $ vsep (map (showASP CausedByR) aspRules) <> line
