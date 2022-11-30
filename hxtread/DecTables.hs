module DecTables where

import ToDMN.Types

testDefs :: Definitions
testDefs =
  Definitions
    { sXmlns = "https://www.omg.org/spec/DMN/20191111/MODEL/"
    , sXmlnsDmndi = "https://www.omg.org/spec/DMN/20191111/DMNDI/"
    , sXmlnsDc = "http://www.omg.org/spec/DMN/20180521/DC/"
    , sXmlnsModeler = "http://camunda.org/schema/modeler/1.0"
    , sXmlnsDi = "http://www.omg.org/spec/DMN/20180521/DI/"
    , sDefId = "Definitions_1"
    , sDefName = "myDRD"
    , sNamespace = "http://camunda.org/schema/1.0/dmn"
    , sExporter = "Camunda Modeler"
    , sExporterVersion = "5.1.0"
    , sModelerExPlat = "Camunda Cloud"
    , sModelerExPlatVer = "8.0.0"
    , sDecisions =
        [ Decision
            { sDecId = "O"
            , sDecName = "O"
            , sDecOutVar = DecOutVar
                { sDecVarId = "InformationItem_1"
                , sDecVarName = "O"
                , sDecVarFEELType = "number"
                }
            , sDecTableInfoReqs =
                [ ReqInputEl
                    { sReqInputId = "InformationRequirement_1"
                    , sReqInput = "#P1"
                    }
                ]
            , sDecTableOrLitExpr = DecTable
                { sDecTableId = "DecisionTable_1"
                , sSchema = Schema
                    { sInputSchemas =
                        [ InputSchema
                            { sInputSchemaId = "InputClause_1"
                            , sInputLabel = Just "optional input label"
                            , sInputExprEl = InputExprEl
                                { sInputExprElId = "LiteralExpression_1"
                                , sInputExprFEELType = "number"
                                , sInputExprVarName = XMLText { sText = "P1" }
                                }
                            }
                        ]
                    , sOutputSchema = OutputSchema
                        { sOutputSchemaId = "OutputClause_1"
                        , sOutputLabel = Just "optional output label"
                        , sOutputSchemaVarName = "O"
                        , sOutputSchemaFEELType = "number"
                        }
                    }
                , sRules =
                    [ DMNRule
                        { sRuleId = "DecisionRule_1"
                        , sInputEntries =
                            [ InputEntry
                                { sInputEntryId = "UnaryTests_1"
                                , sMaybeCondition = Just
                                    ( XMLText { sText = "1" } )
                                }
                            ]
                        , sOutputEntry = OutputEntry
                            { sOutputId = "LiteralExpression_2"
                            , sExpr = XMLText { sText = "10" }
                            }
                        }
                    ]
                }
            }
        , Decision
            { sDecId = "P1"
            , sDecName = "P1"
            , sDecOutVar = DecOutVar
                { sDecVarId = "InformationItem_2"
                , sDecVarName = "P1"
                , sDecVarFEELType = "number"
                }
            , sDecTableInfoReqs = []
            , sDecTableOrLitExpr = DecTable
                { sDecTableId = "DecisionTable_2"
                , sSchema = Schema
                    { sInputSchemas = []
                    , sOutputSchema = OutputSchema
                        { sOutputSchemaId = "OutputClause_2"
                        , sOutputLabel = Just "optional output label"
                        , sOutputSchemaVarName = "P1"
                        , sOutputSchemaFEELType = "number"
                        }
                    }
                , sRules =
                    [ DMNRule
                        { sRuleId = "DecisionRule_2"
                        , sInputEntries = []
                        , sOutputEntry = OutputEntry
                            { sOutputId = "LiteralExpression_3"
                            , sExpr = XMLText { sText = "1" }
                            }
                        }
                    ]
                }
            }
        ]
    }
