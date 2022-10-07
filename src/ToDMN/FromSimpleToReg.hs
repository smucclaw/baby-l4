
module ToDMN.FromSimpleToReg where

import ToDMN.Types
import Text.StringRandom
import qualified System.Random as Random
import qualified Data.Text as T

mkID :: String -> String
mkID pfx = T.unpack $ stringRandom (Random.mkStdGen 137) (T.pack (pfx ++ "_([0-9]|[a-z]|[A-Z]){7}"))

sDecisionToDecision :: SimpleDecision -> Decision
sDecisionToDecision (SimpleDecTableEl sInfoReqs simSchema sDMNRules) =
  let sch = sSchemaToSchema simSchema
      outsch = sOutputSchema sch
      outname = sOutputSchemaVarName outsch
  in Decision outname outname $ DecTable (mkID "DecisionTable") (map sInfoReqToInfoReq sInfoReqs) (sSchemaToSchema simSchema) (map sDMNRuleToDMNRule sDMNRules)
sDecisionToDecision SimpleLitExprEl {} = error "not yet implemented"

sInfoReqToInfoReq :: SimpleInfoReq -> InfoReq
sInfoReqToInfoReq (SimpleReqInputEl reqInput) =
  ReqInputEl (mkID "InformationRequirement") reqInput

sSchemaToSchema :: SimpleSchema -> Schema
sSchemaToSchema (SimpleSchema sInputSchemas sOutputSchema) =
  Schema (map sInputSchemaToInputSchema sInputSchemas) (sOutputSchemaToOutputSchema sOutputSchema)

sInputSchemaToInputSchema :: SimpleInputSchema -> InputSchema
sInputSchemaToInputSchema (SimpleInputSchema sInpExprVarName sInpExprFEELType) =
  InputSchema (mkID "InputClause") (Just "optional input label") (InputExprEl (mkID "LiteralExpression") sInpExprFEELType (XMLText sInpExprVarName))


sOutputSchemaToOutputSchema :: SimpleOutputSchema -> OutputSchema
sOutputSchemaToOutputSchema (SimpleOutputSchema sOutSchemaVarName sOutSchemaFEELType) =
  OutputSchema (mkID "OutputClause") (Just "optional output label") sOutSchemaVarName sOutSchemaFEELType

sDMNRuleToDMNRule :: SimpleDMNRule -> DMNRule
sDMNRuleToDMNRule (SimpleDMNRule sInpEntries sOutputEntry) =
  DMNRule (mkID "DecisionRule") (map sInputEntryToInputEntry sInpEntries) (sOutputEntryToOutputEntry sOutputEntry)

sInputEntryToInputEntry :: SimpleInputEntry -> InputEntry
sInputEntryToInputEntry (SimpleInputEntry mCondition) = InputEntry (mkID "UnaryTests") mCondition

sOutputEntryToOutputEntry :: SimpleOutputEntry -> OutputEntry
sOutputEntryToOutputEntry (SimpleOutputEntry feelExpr) = OutputEntry (mkID "LiteralExpression") feelExpr
