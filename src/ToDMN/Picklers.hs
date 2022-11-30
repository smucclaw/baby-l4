module ToDMN.Picklers where

import System.Environment
import Text.XML.HXT.Core
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import ToDMN.Types
import L4.Syntax

-- Utils
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f ~(a, b, c, d, e) = f a b c d e

uncurry13 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n)
          -> (a, b, c, d, e, f, g, h, i, j, k, l, m)
          -> n
uncurry13 fn ~(a, b, c, d, e, f, g, h, i, j, k, l, m) = fn a b c d e f g h i j k l m

----------------------------------------------
-- Some fake data types for experimenting with XML picklers

newtype Bar = Bar {sBar :: String}
  deriving (Show, Eq)
data Foo = Foo {sFooNum :: Int, sFooBars :: [Bar] }
  deriving (Show, Eq)

instance XmlPickler Bar where
  xpickle = xpBar

xpBar :: PU Bar
xpBar = xpElem "Bar" $ xpWrap (Bar, \ (Bar b) -> b) xpText

instance XmlPickler Foo where
  xpickle = xpFoo

xpFoo :: PU Foo
xpFoo = xpElem "Foo" $
          xpWrap ( uncurry Foo
                ,  \f -> (sFooNum f, sFooBars f) ) $
          xpPair (xpAttr "Num" xpInt) (xpList xpBar)


-- Now for the real types

instance XmlPickler InputEntry where
  xpickle = xpInputEntry

xpInputEntry :: PU InputEntry
xpInputEntry = xpElem "inputEntry" $
               xpWrap ( uncurry InputEntry
                      , \ip -> (sInputEntryId ip, sMaybeCondition ip)) $
               xpPair (xpAttr "id" xpText) (xpOption xpXMLText)


-- This doesn't work because HXT requires a perfect match between the
-- content of an XML element and the constituents of a datatype
-- See https://wiki.haskell.org/HXT/Conversion_of_Haskell_data_from/to_XML#A_few_words_of_advice

-- instance XmlPickler Condition where
--   xpickle = xpAlt tag ps where
--     tag (CompVal _ _) = 0
--     tag (CompVar _ _) = 1
--     ps = [ xpWrap (uncurry CompVal, \(CompVal o v) -> (o, v)) $
--            ( xpElem "text" $
--              xpAttr "" $
--              xpickle )
--          , xpWrap (uncurry CompVar, \(CompVar o v) -> (o, v)) $
--            ( xpElem "text" $
--              xpAttr "" $
--              xpickle )
--          ]


-- On Text nodes: the Text interface represents the textual content of an Element or Attr

instance XmlPickler XMLText where
  xpickle = xpXMLText

xpXMLText :: PU XMLText
xpXMLText = xpElem "text" $ xpWrap (XMLText, \ (XMLText b) -> b) xpText


instance XmlPickler OutputEntry where
  xpickle = xpOutputEntry

xpOutputEntry :: PU OutputEntry
xpOutputEntry = xpElem "outputEntry" $
               xpWrap ( uncurry OutputEntry
                      , \op -> (sOutputId op, sExpr op) ) $
               xpPair (xpAttr "id" xpText) xpXMLText

instance XmlPickler DMNRule where
  xpickle = xpDMNRule

xpDMNRule :: PU DMNRule
xpDMNRule = xpElem "rule" $
            xpWrap ( uncurry3 DMNRule
                   , \r -> (sRuleId r, sInputEntries r, sOutputEntry r) ) $
            xpTriple (xpAttr "id" xpText) (xpList xpInputEntry) xpOutputEntry


-- Input Expression Element
instance XmlPickler InputExprEl where
  xpickle = xpInputExprEl

xpInputExprEl :: PU InputExprEl
xpInputExprEl = xpElem "inputExpression" $
                xpWrap ( uncurry3 InputExprEl
                       , \ie -> (sInputExprElId ie, sInputExprFEELType ie, sInputExprVarName ie) ) $
                xpTriple (xpAttr "id" xpText) (xpAttr "typeRef" xpText) xpXMLText

-- Input Schema
instance XmlPickler InputSchema where
  xpickle = xpInputSchema

xpInputSchema :: PU InputSchema
xpInputSchema = xpElem "input" $
                xpWrap ( uncurry3 InputSchema
                       , \s -> (sInputSchemaId s, sInputLabel s, sInputExprEl s) ) $
                xpTriple (xpAttr "id" xpText) (xpAttrImplied "label" xpText) xpInputExprEl

-- xpAttrImplied :: String -> PU a -> PU (Maybe a)

instance XmlPickler OutputSchema where
  xpickle = xpOutputSchema

xpOutputSchema :: PU OutputSchema
xpOutputSchema = xpElem "output" $
                 xpWrap ( uncurry4 OutputSchema
                        , \s -> (sOutputSchemaId s, sOutputLabel s, sOutputSchemaVarName s, sOutputSchemaFEELType s) ) $
                 xp4Tuple (xpAttr "id" xpText) (xpAttrImplied "label" xpText) (xpAttr "name" xpText)
                 (xpAttr "typeRef" xpText)

instance XmlPickler ToDMN.Types.Schema where
  xpickle = xpSchema

xpSchema :: PU ToDMN.Types.Schema
xpSchema = xpWrap ( uncurry Schema
                  , \s -> (sInputSchemas s, sOutputSchema s) ) $
           xpPair (xpList xpInputSchema) xpOutputSchema

instance XmlPickler InfoReq where
  xpickle = xpInfoReq

xpInfoReq :: PU InfoReq
xpInfoReq = xpElem "informationRequirement" $
            xpWrap ( uncurry ReqInputEl
                   , \i -> (sReqInputId i, sReqInput i) ) $
            xpPair (xpAttr "id" xpText) (xpElem "requiredDecision" (xpAttr "href" xpText))


instance XmlPickler Decision where
  xpickle = xpDecision
    -- xpElem "decision" $
    -- xpAlt tag ps
    -- where
    --   tag (LitExprEl _ _ _ _ _) = 0
    --   tag (DecTableEl _ _ _ _ _) = 1
    --   ps = [ xpElem "literalExpression" $
    --          xpWrap ( uncurry5 LitExprEl
    --                 , \e -> (e, e, e, e, e) ) $
    --          xp5Tuple (xpAttr "id" xpText) (xpAttr "label" xpText) (xpList xpInfoReq)
    --        -- for now
    --        , xpElem "decisionTable" $
    --          xpWrap ( uncurry5 DecTableEl
    --                 , \t -> (sDecTableId t, sDecTableLabel t, sDecTableInfoReqs t, sSchema t, sRules t) ) $
    --          xp5Tuple (xpAttr "id" xpText) (xpAttr "label" xpText) (xpList xpInfoReq) xpSchema (xpList xpDMNRule)
    --        ]

instance XmlPickler DecTableOrLitExpr where
  xpickle = xpDecTableOrLitExpr

xpDecTableOrLitExpr :: PU DecTableOrLitExpr
xpDecTableOrLitExpr = xpElem "decisionTable" $
                      xpWrap ( uncurry3 DecTable
                             , \t -> ( sDecTableId t
                                     , sSchema t
                                     , sRules t ) ) $
                      xpTriple (xpAttr "id" xpText) xpSchema (xpList xpDMNRule)

xpDecision :: PU Decision
xpDecision = xpElem "decision" $
             xpWrap ( uncurry5 Decision
                    , \d -> ( sDecId d
                            , sDecName d
                            , sDecOutVar d
                            , sDecTableInfoReqs d
                            , sDecTableOrLitExpr d )) $
             xp5Tuple (xpAttr "id" xpText) (xpAttr "name" xpText) xpDecOutVar (xpList xpInfoReq) xpDecTableOrLitExpr

instance XmlPickler DecOutVar where
  xpickle = xpDecOutVar

xpDecOutVar :: PU DecOutVar
xpDecOutVar = xpElem "variable" $
              xpWrap ( uncurry3 DecOutVar
                     , \v -> ( sDecVarId v
                             , sDecVarName v
                             , sDecVarFEELType v )) $
              xpTriple (xpAttr "id" xpText) (xpAttr "name" xpText) (xpAttr "typeRef" xpText)


xpDefinitions :: PU Definitions
xpDefinitions = xpElem "definitions" $
                xpWrap ( uncurry13 Definitions
                       , \d -> ( sXmlns d
                               , sXmlnsDmndi d
                               , sXmlnsDc d
                               , sXmlnsModeler d
                               , sXmlnsDi d
                               , sDefId d
                               , sDefName d
                               , sNamespace d
                               , sExporter d
                               , sExporterVersion d
                               , sModelerExPlat d
                               , sModelerExPlatVer d
                               , sDecisions d )) $
                xp13Tuple ( xpAttr "xmlns" xpText )
                          ( xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "dmndi" xpText )
                          ( xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "dc" xpText )
                          ( xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "modeler" xpText )
                          ( xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "di" xpText )
                          ( xpAttr "id" xpText )
                          ( xpAttr "name" xpText )
                          ( xpAttr "namespace" xpText )
                          ( xpAttr "exporter" xpText )
                          ( xpAttr "exporterVersion" xpText )
                          ( xpAttrNS "http://camunda.org/schema/1.0/dmn" "modeler" "executionPlatform" xpText )
                          ( xpAttrNS "http://camunda.org/schema/1.0/dmn" "modeler" "executionPlatformVersion" xpText )
                          ( xpList xpDecision )


-- Tests
testInputE :: InputEntry
testInputE = InputEntry "UnaryTests_0w2546x" (Just (XMLText "< x"))

testOutputE1 :: OutputEntry
testOutputE1 = OutputEntry "LiteralExpression_174qejr" (XMLText "true")

testOutputE2 :: OutputEntry
testOutputE2 = OutputEntry "LiteralExpression_0qis36e" (XMLText "yes")

testDMNRule :: DMNRule
testDMNRule = DMNRule "DecisionRule_1080bsl" [testInputE] testOutputE1

testInputExprEl :: InputExprEl
testInputExprEl = InputExprEl "LiteralExpression_0lo9u0r" "number" (XMLText "minIncome")

testInputSchema :: InputSchema
testInputSchema = InputSchema "InputClause_1051ttc" (Just "MinIncome") testInputExprEl

testOutputSchema :: OutputSchema
testOutputSchema = OutputSchema "OutputClause_1kahfkg" Nothing "savings_adequacy" "string"

testInfoReq :: InfoReq
testInfoReq = ReqInputEl "InformationRequirement_0cndp0l" "#two_x"

testSchema :: ToDMN.Types.Schema
testSchema = ToDMN.Types.Schema [testInputSchema] testOutputSchema
