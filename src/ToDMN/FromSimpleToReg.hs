
module ToDMN.FromSimpleToReg where

import ToDMN.Types

sDecisionToDecision :: SimpleDecision -> Decision
sDecisionToDecision (SimpleDecTableEl sInfoReqs sSchema sDMNRules) =
  DecTableEl "id" "label" (map sInfoReqToInfoReq sInfoReqs) (sSchemaToSchema sSchema) (map sDMNRuleToDMNRule sDMNRules)
sDecisionToDecision SimpleLitExprEl {} = error "not yet implemented"

sInfoReqToInfoReq :: SimpleInfoReq -> InfoReq
sInfoReqToInfoReq (SimpleReqInputEl reqInput) =
  ReqInputEl "id" reqInput

sSchemaToSchema :: SimpleSchema -> Schema
sSchemaToSchema (SimpleSchema sInputSchemas sOutputSchema) =
  Schema (map sInputSchemaToInputSchema sInputSchemas) (sOutputSchemaToOutputSchema sOutputSchema)

sInputSchemaToInputSchema :: SimpleInputSchema -> InputSchema
sInputSchemaToInputSchema (SimpleInputSchema sInpExprVarName sInpExprFEELType) =
  InputSchema "id" (Just "label") (InputExprEl "id" sInpExprFEELType (XMLText sInpExprVarName))


sOutputSchemaToOutputSchema :: SimpleOutputSchema -> OutputSchema
sOutputSchemaToOutputSchema (SimpleOutputSchema sOutSchemaVarName sOutSchemaFEELType) =
  OutputSchema "label" sOutSchemaVarName sOutSchemaFEELType

sDMNRuleToDMNRule :: SimpleDMNRule -> DMNRule
sDMNRuleToDMNRule (SimpleDMNRule sInpEntries sOutputEntry) =
  DMNRule "id" (map sInputEntryToInputEntry sInpEntries) (sOutputEntryToOutputEntry sOutputEntry)

sInputEntryToInputEntry :: SimpleInputEntry -> InputEntry
sInputEntryToInputEntry (SimpleInputEntry mCondition) = InputEntry "id" mCondition

sOutputEntryToOutputEntry :: SimpleOutputEntry -> OutputEntry
sOutputEntryToOutputEntry (SimpleOutputEntry feelExpr) = OutputEntry "id" feelExpr
