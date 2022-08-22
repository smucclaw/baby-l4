module Main where

import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import ToDMN.Types
import L4.Syntax


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
                      , \ip -> (sInputId ip, sMaybeCondition ip)) $
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
                   , \r -> (sRuleId r, sInputEntries r, sOutputEntries r) ) $
            xpTriple (xpAttr "id" xpText) (xpList xpInputEntry) (xpList xpOutputEntry)


-- xpFoo :: PU Foo
-- xpFoo = xpElem "Foo" $
--           xpWrap ( uncurry Foo
--                 ,  \f -> (sFooNum f, sFooBars f) ) $
--           xpPair (xpAttr "Num" xpInt) (xpList xpBar)


-- Tests
testInputE :: InputEntry
testInputE = InputEntry "UnaryTests_0w2546x" (Just (XMLText "< x"))

testOutputE1 :: OutputEntry
testOutputE1 = OutputEntry "LiteralExpression_174qejr" (XMLText "true")

testOutputE2 :: OutputEntry
testOutputE2 = OutputEntry "LiteralExpression_0qis36e" (XMLText "yes")

testDMNRule :: DMNRule
testDMNRule = DMNRule "DecisionRule_1080bsl" [testInputE] [testOutputE1, testOutputE2]

{-
>>> uncurry Foo (42, [Bar "b1", Bar "b2"])
Foo {sFooNum = 42, sFooBars = [Bar {sBar = "b1"},Bar {sBar = "b2"}]}

>>> (\f -> (sFooNum f, sFooBars f)) (Foo {sFooNum = 42, sFooBars = [Bar {sBar = "b1"},Bar {sBar = "b2"}]})
(42,[Bar {sBar = "b1"},Bar {sBar = "b2"}])
-}

{-
foo :: IOStateArrow s0 Bar XmlTree
foo = xpickleDocument xpBar [] "foo"

flub :: IO [XmlTree]
flub = runX foo

simplePickler :: IO ()
simplePickler = do
  [src, dst] <- getArgs
  runX $
    (xpickleDocument xpBar [withIndent yes,
                           withOutputEncoding isoLatin1
                  ] dst)
  return ()

-}

main2 :: IO ()
main2
    = do
      runX (
        -- constA (Foo 42 [Bar "b1", Bar "b2"])
        -- constA (XMLText "hello")
        -- constA testInputE
        -- constA testOutputE
        constA testDMNRule
        >>>
        xpickleDocument        xpDMNRule -- xpFoo
                               [ withIndent yes
                               ] "main2testout.xml" -- "main2out.xml"
          )
      return ()


----------------------------------------------

main :: IO ()
main = main2
{-
main = do
  argv <- getArgs
  (al, src, dst) <- cmdlineOpts argv
  [rc]  <- runX (application al src dst)
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess
-}

cmdlineOpts     :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv = return ([withValidate no], argv!!0, argv!!1)

application     :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst =
  configSysVars (withTrace 1 : cfg)
  >>> readDocument [ -- withParseHTML yes,
                   withValidate no
                   ,withCurl []
                   ] src
  -- >>> withTraceLevel 4 (traceDoc "resulting document")      -- (1)
  -- >>> processChildren (processDocumentRootElement `when` isElem)  -- (1)
  -- >>> withTraceLevel 4 (traceDoc "after processing")      -- (1)
  >>> processFullDocument
  >>> writeDocument [
  --withIndent yes
  -- ,withOutputEncoding isoLatin1
                    ] dst                                        -- (3)
  >>> getErrStatus

processFullDocument :: IOSArrow XmlTree XmlTree
processFullDocument = arrIO (\ x -> do {print "full doc:"; print x; return x})

processDocumentRootElement :: IOSArrow XmlTree XmlTree
processDocumentRootElement
    = selectAllP


selectAllText :: ArrowXml a => a XmlTree XmlTree
selectAllText
   = deep isText

selectAllP :: ArrowXml a => a XmlTree XmlTree
selectAllP
    = deep
      ( isText                       -- (1)
        <+>
        ( isElem >>> hasName "a"   -- (2)
          >>> getAttrValue "href"         -- (3)
          >>> mkText                     -- (4)
        )
      )



