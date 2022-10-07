module DecTables where

import ToDMN.Types

testDecision :: [Decision]
testDecision =
  [ Decision
    { sDecId = "O"
    , sDecName = "O"
    , sDecTableOrLitExpr = DecTable
        { sDecTableId = "DecisionTable_sNwgo15"
        , sDecTableInfoReqs =
            [ ReqInputEl
                { sReqInputId = "InformationRequirement_8W7Dch7"
                , sReqInput = "P1"
                }
            ]
        , sSchema = Schema
            { sInputSchemas =
                [ InputSchema
                    { sInputSchemaId = "InputClause_1sNwgo1"
                    , sInputLabel = Just "optional input label"
                    , sInputExprEl = InputExprEl
                        { sInputExprElId = "LiteralExpression_wgo15L1"
                        , sInputExprFEELType = Number
                        , sInputExprVarName = XMLText { sText = "P1" }
                        }
                    }
                ]
            , sOutputSchema = OutputSchema
                { sOutputSchemaId = "OutputClause_6G1xr8W"
                , sOutputLabel = Just "optional output label"
                , sOutputSchemaVarName = "O"
                , sOutputSchemaFEELType = Number
                }
            }
        , sRules =
            [ DMNRule
                { sRuleId = "DecisionRule_6G1xr8W"
                , sInputEntries =
                    [ InputEntry
                        { sInputEntryId = "UnaryTests_66G1xr8"
                        , sMaybeCondition = Just
                            ( XMLText { sText = "1" } )
                        }
                    ]
                , sOutputEntry = OutputEntry
                    { sOutputId = "LiteralExpression_wgo15L1"
                    , sExpr = XMLText { sText = "10" }
                    }
                }
            ]
        }
    }
  , Decision
    { sDecId = "P1"
    , sDecName = "P1"
    , sDecTableOrLitExpr = DecTable
        { sDecTableId = "DecisionTable_sNwgo15"
        , sDecTableInfoReqs = []
        , sSchema = Schema
            { sInputSchemas = []
            , sOutputSchema = OutputSchema
                { sOutputSchemaId = "OutputClause_6G1xr8W"
                , sOutputLabel = Just "optional output label"
                , sOutputSchemaVarName = "P1"
                , sOutputSchemaFEELType = Number
                }
            }
        , sRules =
            [ DMNRule
                { sRuleId = "DecisionRule_6G1xr8W"
                , sInputEntries = []
                , sOutputEntry = OutputEntry
                    { sOutputId = "LiteralExpression_wgo15L1"
                    , sExpr = XMLText { sText = "1" }
                    }
                }
            ]
        }
    }
  ]
