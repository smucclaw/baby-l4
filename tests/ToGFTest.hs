module ToGFTest where

import Syntax
import Annotation
import ToGF.FromL4.ToAnswers ( getAtoms )
import ToGF.GenerateLexicon ( AtomWithArity(AA) )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

-- Tests the function getAtoms from ToGF.FromL4.ToAnswers
testGetAtoms :: TestTree
testGetAtoms = testGroup "Test getAtoms"
  [ testCase "Binary function that returns an Int)" $
      getAtoms vardecl @?=
        [AA "foo" 2, AA "Business" 0, AA "Person" 0]
  ]
  where
    vardecl = VarDecl
            { annotOfVarDecl = LocTypeAnnot
                { locAnnot = RealSRng
                    ( SRng
                        { start = Pos
                            { line = 29
                            , col = 0
                            }
                        , end = Pos
                            { line = 30
                            , col = 31
                            }
                        }
                    )
                , typeAnnot = OkT
                }
            , nameOfVarDecl = "foo"
            , tpOfVarDecl = FunT
                ( ClassT
                    ( ClsNm { stringOfClassName = "Business" } )
                )
                ( FunT
                    ( ClassT
                        ( ClsNm { stringOfClassName = "Person" } )
                    ) IntT
                )
            }
testDescription :: TestTree
testDescription = testGroup  "Test the parseDescription"
    [testCase "parse gives the Descrption" $
        parseDescription  "{Player} participates in {Game}" @?= Descr "participates in" ["Player", "Game"]
    ,
    testCase "parse with no arguments" $
        parseDescription  "I am super high" @?= Descr "I am super high" []
    ]