
module ToDMN.FromSimpleToReg where

import ToDMN.Types
import Text.StringRandom
import qualified Data.Text as T

mkID :: String -> IO String
mkID pfx = do
  idIO <- stringRandomIO (T.pack (pfx ++ "_([0-9]|[a-z]|[A-Z]){7}"))
  return $ T.unpack idIO

sDecisionToDecision :: SimpleDecision -> IO Decision
sDecisionToDecision (SimpleDecTableEl sInfoReqs simSchema sDMNRules) = do
  sch         <- sSchemaToSchema simSchema
  let outsch  =  sOutputSchema sch
  let outname =  sOutputSchemaVarName outsch
  iddt        <- mkID "DecisionTable"
  sinforeqs   <- mapM sInfoReqToInfoReq sInfoReqs
  sdmns       <- mapM sDMNRuleToDMNRule sDMNRules
  return $ Decision outname outname $ DecTable iddt sinforeqs sch sdmns
sDecisionToDecision SimpleLitExprEl {} = error "not yet implemented"

sInfoReqToInfoReq :: SimpleInfoReq -> IO InfoReq
sInfoReqToInfoReq (SimpleReqInputEl reqInput) = do
  idir <- mkID "InformationRequirement"
  return $ ReqInputEl idir reqInput

sSchemaToSchema :: SimpleSchema -> IO Schema
sSchemaToSchema (SimpleSchema sInputSchemas' sOutputSchema') = do
  Schema
    <$> mapM sInputSchemaToInputSchema sInputSchemas'
    <*> sOutputSchemaToOutputSchema sOutputSchema'

sInputSchemaToInputSchema :: SimpleInputSchema -> IO InputSchema
sInputSchemaToInputSchema (SimpleInputSchema sInpExprVarName sInpExprFEELType) = do
  idic <- mkID "InputClause"
  idle <- mkID "LiteralExpression"
  return $
    InputSchema idic (Just "optional input label")
    (InputExprEl idle sInpExprFEELType (XMLText sInpExprVarName))

sOutputSchemaToOutputSchema :: SimpleOutputSchema -> IO OutputSchema
sOutputSchemaToOutputSchema (SimpleOutputSchema sOutSchemaVarName sOutSchemaFEELType) = do
  idoc <- mkID "OutputClause"
  return $ OutputSchema idoc (Just "optional output label") sOutSchemaVarName sOutSchemaFEELType

sDMNRuleToDMNRule :: SimpleDMNRule -> IO DMNRule
sDMNRuleToDMNRule (SimpleDMNRule sInpEntries sOutputEntry') = do
  DMNRule
    <$> mkID "DecisionRule"
    <*> mapM sInputEntryToInputEntry sInpEntries
    <*> sOutputEntryToOutputEntry sOutputEntry'

sInputEntryToInputEntry :: SimpleInputEntry -> IO InputEntry
sInputEntryToInputEntry (SimpleInputEntry mCondition) = do
  idut <- mkID "UnaryTests"
  return $ InputEntry idut mCondition

sOutputEntryToOutputEntry :: SimpleOutputEntry -> IO OutputEntry
sOutputEntryToOutputEntry (SimpleOutputEntry feelExpr) = do
  OutputEntry <$> mkID "LiteralExpression" <*> pure feelExpr
