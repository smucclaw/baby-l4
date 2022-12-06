
module Proof(proveProgram) where
import L4.Syntax
    ( assertionsOfProgram,
      Assertion(instrOfAssertion),
      Program(elementsOfProgram),
      Tp,
      TopLevelElement(RuleTLE),
      getRule,
      typeOfTLE,
      rulesOfProgram )
import ToASP(proveAssertionASP)
import TimedMC (proveAssertionTA)
import CtlMC (proveAssertionCTL)
import Smt(proveAssertionSMT)
import Control.Monad (foldM)
import RuleTransfo (rewriteRuleSetDerived, rewriteRuleSetSubjectTo, rewriteRuleSetDespite)
import L4.PrintProg
    ( renameAndPrintRule, namesUsedInProgram, printTest, PrintSystem (UppaalStyle, L4Style) )



proveAssertion :: Program (Tp ()) -> Assertion (Tp ()) -> IO ()
proveAssertion p asrt = foldM (\r (k,instr) ->
            case k of
              "CTL" -> proveAssertionCTL p instr asrt
              "SMT" -> proveAssertionSMT p instr asrt
              "sCASP"-> proveAssertionASP p instr asrt
              "TA" -> proveAssertionTA p instr asrt
              "printUp" -> printTest p UppaalStyle
              "printL4" -> printTest p L4Style
              _ -> return ())
          () (instrOfAssertion asrt)

proveProgram :: Program (Tp ()) -> IO ()
proveProgram p = do
  let transfRules = rewriteRuleSetDerived (rewriteRuleSetSubjectTo (rewriteRuleSetDespite (rulesOfProgram p)))
  let updRules = [e | e <- elementsOfProgram p, not (typeOfTLE getRule e)] ++ map RuleTLE transfRules
  let transfProg = p{elementsOfProgram = updRules}
  putStrLn "Generated rules:"
  putStrLn (concatMap (renameAndPrintRule (namesUsedInProgram transfProg)) transfRules)
  foldM (\r a -> proveAssertion transfProg a) () (assertionsOfProgram transfProg)
