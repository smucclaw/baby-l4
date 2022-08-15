module Main where

import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))


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
        constA (Foo 42 [Bar "b1", Bar "b2"])
        >>>
        xpickleDocument   xpFoo
                               [ withIndent yes
                               ] "main2out.xml"
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



