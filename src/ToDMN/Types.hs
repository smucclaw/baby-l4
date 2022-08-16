
module ToDMN.Types where

import L4.Syntax (BComparOp, Val)

type Id = String
type VarName = String
type Label = String

-- TODO
type FEELExpr = String

data DecOutVar = DecOutVar Id VarName FEELType

-- Label is tagged name in a decision element
-- TODO: crosscheck with DMN standard to see if classification scheme matches
data Decision
  = LitExprEl Id Label DecOutVar [InfoReq] FEELExpr
  | DecTableEl Id Label Schema [Rule]

data Schema = Schema [InputSchema] [OutputSchema]

-- TypeRef is a String representation of FEELExpr types
-- The FEELExpr types enumerated here only reflect the possible options on Camunda Modeler
-- The docs on FEEL data types lists an additional 2 types: lists, maps
-- Other types of FEEL expressions listed on the docs are: variables, control flow, functions
-- See https://docs.camunda.io/docs/components/modeler/feel/language-guide/feel-expressions-introduction
-- Maybe this is what the Modeler Any type covers?
data InputExprEl = InputExprEl Id FEELType VarName
data FEELType
  = String
  | Bool
  | Number
  -- | DateTime
  -- | DayTimeDuration
  -- | YearMonthDuration

-- Table Headers
-- Label is tagged label in input/output elements
data InputSchema = InputSchema Id Label InputExprEl
data OutputSchema = OutputSchema Label VarName FEELType

type DRD = [Decision]

data InfoReq = ReqInputEl Id ReqInput
type ReqInput = String

data Rule = Rule Id [InputEntry] [OutputEntry]

data InputEntry = InputEntry { sId :: Id, sMaybeCondition :: Maybe Condition }

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
type Condition = XMLText


-- Conclusion type is constrained by OutputSchema TypeRef? Which is a String representation of FEELExpr types
data OutputEntry = OutputEntry Id FEELExpr

-- to investigate: how much of this DMN syntax is part of the DMN standard, and how much is Camunda's implementation?
