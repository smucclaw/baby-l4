module DecTables where

import ToDMN.Types

testDecision :: [Decision]
testDecision =
  [ DecTableEl
    { sDecTableId = "id"
    , sDecTableLabel = "label"
    , sDecTableInfoReqs =
        [ ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "P1"
            }
        ]
    , sSchema = Schema
        { sInputSchemas =
            [ InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Number
                    , sInputExprVarName = XMLText { sText = "P1" }
                    }
                }
            ]
        , sOutputSchema = OutputSchema
            { sOutputLabel = "label"
            , sOutputSchemaVarName = "I"
            , sOutputSchemaFEELType = Number
            }
        }
    , sRules =
        [ DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "9" } )
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "5" }
                }
            }
        , DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Nothing
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "5" }
                }
            }
        ]
    }
  , DecTableEl
    { sDecTableId = "id"
    , sDecTableLabel = "label"
    , sDecTableInfoReqs =
        [ ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "P1"
            }
        , ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "P3"
            }
        , ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "P2"
            }
        ]
    , sSchema = Schema
        { sInputSchemas =
            [ InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Number
                    , sInputExprVarName = XMLText { sText = "P1" }
                    }
                }
            , InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Number
                    , sInputExprVarName = XMLText { sText = "P3" }
                    }
                }
            , InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Bool
                    , sInputExprVarName = XMLText { sText = "P2" }
                    }
                }
            ]
        , sOutputSchema = OutputSchema
            { sOutputLabel = "label"
            , sOutputSchemaVarName = "O"
            , sOutputSchemaFEELType = Number
            }
        }
    , sRules =
        [ DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "2" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "4" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "False" } )
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "11" }
                }
            }
        , DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "1" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Nothing
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Nothing
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "10" }
                }
            }
        , DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "11" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "33" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "True" } )
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "44" }
                }
            }
        ]
    }
  , DecTableEl
    { sDecTableId = "id"
    , sDecTableLabel = "label"
    , sDecTableInfoReqs =
        [ ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "P1"
            }
        , ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "P2"
            }
        , ReqInputEl
            { sReqInputId = "id"
            , sReqInput = "I"
            }
        ]
    , sSchema = Schema
        { sInputSchemas =
            [ InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Number
                    , sInputExprVarName = XMLText { sText = "P1" }
                    }
                }
            , InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Bool
                    , sInputExprVarName = XMLText { sText = "P2" }
                    }
                }
            , InputSchema
                { sInputSchemaId = "id"
                , sInputLabel = Just "label"
                , sInputExprEl = InputExprEl
                    { sInputExprElId = "id"
                    , sInputExprFEELType = Number
                    , sInputExprVarName = XMLText { sText = "I" }
                    }
                }
            ]
        , sOutputSchema = OutputSchema
            { sOutputLabel = "label"
            , sOutputSchemaVarName = "O2"
            , sOutputSchemaFEELType = Number
            }
        }
    , sRules =
        [ DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "1" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "True" } )
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Nothing
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "10" }
                }
            }
        , DMNRule
            { sRuleId = "id"
            , sInputEntries =
                [ InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Nothing
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Nothing
                    }
                , InputEntry
                    { sInputEntryId = "id"
                    , sMaybeCondition = Just
                        ( XMLText { sText = "5" } )
                    }
                ]
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "8" }
                }
            }
        ]
    }
  , DecTableEl
    { sDecTableId = "id"
    , sDecTableLabel = "label"
    , sDecTableInfoReqs = []
    , sSchema = Schema
        { sInputSchemas = []
        , sOutputSchema = OutputSchema
            { sOutputLabel = "label"
            , sOutputSchemaVarName = "P1"
            , sOutputSchemaFEELType = Number
            }
        }
    , sRules =
        [ DMNRule
            { sRuleId = "id"
            , sInputEntries = []
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "1" }
                }
            }
        ]
    }
  , DecTableEl
    { sDecTableId = "id"
    , sDecTableLabel = "label"
    , sDecTableInfoReqs = []
    , sSchema = Schema
        { sInputSchemas = []
        , sOutputSchema = OutputSchema
            { sOutputLabel = "label"
            , sOutputSchemaVarName = "P2"
            , sOutputSchemaFEELType = Bool
            }
        }
    , sRules =
        [ DMNRule
            { sRuleId = "id"
            , sInputEntries = []
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "True" }
                }
            }
        ]
    }
  , DecTableEl
    { sDecTableId = "id"
    , sDecTableLabel = "label"
    , sDecTableInfoReqs = []
    , sSchema = Schema
        { sInputSchemas = []
        , sOutputSchema = OutputSchema
            { sOutputLabel = "label"
            , sOutputSchemaVarName = "P3"
            , sOutputSchemaFEELType = Number
            }
        }
    , sRules =
        [ DMNRule
            { sRuleId = "id"
            , sInputEntries = []
            , sOutputEntry = OutputEntry
                { sOutputId = "id"
                , sExpr = XMLText { sText = "33" }
                }
            }
        ]
    }
  ]
