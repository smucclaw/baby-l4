
module ToDMN.Types where

-- import L4.Syntax (BComparOp, Val)

type Id = String
type VarName = XMLText
type Label = String
type DecName = String

-- TODO
-- should FEELExpr be XMLText? see XMLText def below
-- type FEELExpr = String

data DecOutVar = DecOutVar Id VarName FEELType
  deriving Show

-- Label is tagged name in a decision element
-- TODO: crosscheck with DMN standard to see if classification scheme matches
data Decision = Decision
  { sDecId :: Id
  , sDecName :: DecName
  , sDecTableOrLitExpr :: DecTableOrLitExpr }
  deriving Show


data DecTableOrLitExpr =
  DecTable
  { sDecTableId :: Id
  , sDecTableInfoReqs :: [InfoReq]
  , sSchema :: Schema
  , sRules :: [DMNRule] }
  -- | LitExpr Id Label DecOutVar [InfoReq] FEELExpr
  deriving Show


data Schema = Schema
  -- [InputSchema] OutputSchema
  { sInputSchemas :: [InputSchema]
  , sOutputSchema :: OutputSchema }
  deriving Show

-- TypeRef is a String representation of FEELExpr types
-- The FEELExpr types enumerated here only reflect the possible options on Camunda Modeler
-- The docs on FEEL data types lists an additional 2 types: lists, maps
-- Other types of FEEL expressions listed on the docs are: variables, control flow, functions
-- See https://docs.camunda.io/docs/components/modeler/feel/language-guide/feel-expressions-introduction
-- Maybe this is what the Modeler Any type covers?
data InputExprEl = InputExprEl
  { sInputExprElId :: Id
  , sInputExprFEELType :: FEELType
  , sInputExprVarName :: VarName }
  deriving Show

data FEELType =
    String
  | Bool
  | Number
  -- | DateTime
  -- | DayTimeDuration
  -- | YearMonthDuration
  deriving (Show, Read)


-- Table Headers
-- Label is tagged label in input/output elements
data InputSchema = InputSchema
  { sInputSchemaId :: Id
  , sInputLabel :: Maybe Label
  , sInputExprEl :: InputExprEl }
  deriving Show
data OutputSchema = OutputSchema
  { sOutputSchemaId :: Id
  , sOutputLabel :: Maybe Label
  , sOutputSchemaVarName :: String
  , sOutputSchemaFEELType :: FEELType }
  deriving Show

type DRD = [Decision]

data InfoReq = ReqInputEl
  { sReqInputId :: Id
  , sReqInput :: ReqInput }
  -- Id ReqInput
  deriving Show
type ReqInput = String

data DMNRule = DMNRule
  { sRuleId :: Id
  , sInputEntries :: [InputEntry]
  , sOutputEntry :: OutputEntry }
  deriving Show

data InputEntry = InputEntry
  { sInputEntryId :: Id
  , sMaybeCondition :: Maybe Condition }
  deriving Show

-- Condition is a FEEL Unary Test
-- See notes on FEELTypes above
-- TODO: can input expression be <= y + 3?
-- data Condition
--   = CompVal BComparOp Val
--   | CompVar BComparOp VarName
--   -- | Intv
--   -- | Disj
--   -- | Neg
--   -- | FEELExpr

newtype XMLText = XMLText {sText :: String}
  deriving Show

type Condition = XMLText

-- should FEELExpr be XMLText?
type FEELExpr = XMLText


-- Conclusion type is constrained by OutputSchema TypeRef? Which is a String representation of FEELExpr types
-- data OutputEntry = OutputEntry { sOutputId :: Id, sExpr :: FEELExpr }
data OutputEntry = OutputEntry
  { sOutputId :: Id
  , sExpr :: FEELExpr }
  deriving Show

-- to investigate: how much of this DMN syntax is part of the DMN standard, and how much is Camunda's implementation?
-- paste findings from slack threads


-- Constraints:
-- to resolve difficulties around name/id being used interchangeably as variables
-- name and id of a component (where both are required) will be the same
-- tables will only have 1 output column
-- name of output column is the name of the table and its decision ID &
-- a pred P1 will have input expr "P1" and be produced from a table (with name and id) "P1" with output name "P1"

-- Simple Types (for removing ids and names)

newtype SimpleInfoReq = SimpleReqInputEl ReqInput deriving Show

-- when inforeq is an empty list, inforeq element is not generated
data SimpleDecision
  = SimpleLitExprEl DecOutVar [InfoReq] FEELExpr
  | SimpleDecTableEl [SimpleInfoReq] SimpleSchema [SimpleDMNRule]
  deriving Show

data SimpleSchema = SimpleSchema [SimpleInputSchema] SimpleOutputSchema
  deriving Show

data SimpleInputSchema = SimpleInputSchema
  { sSimpleInputExprVarName :: String
  , sSimpleInputExprFEELType :: FEELType }
  deriving Show

data SimpleOutputSchema = SimpleOutputSchema
  { sSimpleOutputSchemaVarName :: String
  , sSimpleOutputSchemaFEELType :: FEELType }
  deriving Show


data SimpleDMNRule = SimpleDMNRule
  { sSimpleInputEntries :: [SimpleInputEntry]
  , sSimpleOutputEntry :: SimpleOutputEntry }
  deriving Show

newtype SimpleInputEntry = SimpleInputEntry
  { sSimpleMaybeCondition :: Maybe Condition }
  deriving Show

newtype SimpleOutputEntry = SimpleOutputEntry
  { sSimpleExpr :: FEELExpr }
  deriving Show


-- desired output
-- to be transferred to test file
schemaO :: SimpleSchema
schemaO =
  SimpleSchema
    [ SimpleInputSchema "P1" Number
    , SimpleInputSchema "P2" Number
    , SimpleInputSchema "P3" Number ]
    ( SimpleOutputSchema "O" Number )

schemaO2 :: SimpleSchema
schemaO2 =
  SimpleSchema
    [ SimpleInputSchema "P1" Number ]
    ( SimpleOutputSchema "O2" Number )


r1 :: SimpleDMNRule
r1 =
  SimpleDMNRule
    [ SimpleInputEntry (Just (XMLText "1")),
      SimpleInputEntry (Just (XMLText "True")),
      SimpleInputEntry Nothing ]
    ( SimpleOutputEntry (XMLText "10"))

r2 :: SimpleDMNRule
r2 =
  SimpleDMNRule
    [ SimpleInputEntry (Just (XMLText "2")),
      SimpleInputEntry (Just (XMLText "4")),
      SimpleInputEntry (Just (XMLText "False")) ]
    ( SimpleOutputEntry (XMLText "11") )

r3 :: SimpleDMNRule
r3 =
  SimpleDMNRule
    [ SimpleInputEntry (Just (XMLText "1"))
    , SimpleInputEntry Nothing
    , SimpleInputEntry Nothing ]
    ( SimpleOutputEntry (XMLText "10") )
