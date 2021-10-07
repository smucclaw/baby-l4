module ToASP where

import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Syntax
import SimpleRules (isRule, condValid, SimpleRule ( .. ), ruleToSimpleRule)
import Data.Either (rights)
import PrintProg (showL4, PrintCurried (MultiArg), PrintConfig (PrintVarCase, PrintCurried), PrintVarCase (Capitalize), printRule)
import RuleTransfo (normalize, ruleDisjL)


class ShowASP x where
    showASP :: x -> Doc ann

instance Show t => ShowASP (Expr t) where
    showASP (ValE _ v) = showL4 [] v
    showASP e = pretty (show e)

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase Capitalize, PrintCurried MultiArg]

instance Show t => ShowASP (SimpleRule t) where
    showASP (SimpleRule rn _vds preconds postcond) =
        pretty "according_to" <>
        parens (pretty rn <> pretty "," <+> showL4 aspPrintConfig postcond) <>
        pretty ":-" <>
        hsep (punctuate comma (map (\pc -> pretty "legally_holds" <> parens (showL4 aspPrintConfig pc)) preconds)) <>
        pretty "."

astToASP :: Program (Tp ()) -> IO ()
astToASP prg = do
    let rules = concatMap ruleDisjL (rulesOfProgram prg)
    putStrLn "Simplified L4 rules:"
    putDoc $ vsep (map (showL4 []) rules) <> line
    let simpleRules = rights (map ruleToSimpleRule rules)
    putStrLn "ASP rules:"
    putDoc $ vsep (map showASP simpleRules) <> line
