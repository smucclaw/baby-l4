module ToGFTest where

import Syntax
    ( ClassName(ClsNm, stringOfClassName),
      Mapping(..),
      VarDecl(..),
      Tp(OkT, FunT, ClassT),
      Description(Descr),
      parseDescription )
import Annotation
import ToGF.NormalizeSyntax
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
            , tpOfVarDecl = FunT dummyAnn
                ( ClassT dummyAnn
                    ( ClsNm { stringOfClassName = "Business" } )
                )
                ( FunT dummyAnn
                    ( ClassT dummyAnn
                        ( ClsNm { stringOfClassName = "Person" } )
                    ) (IntT dummyAnn)
                )
            }

-- | Some dummy-annotation for the types
dummyAnn :: LocTypeAnnot (Tp ())
dummyAnn = LocTypeAnnot {locAnnot = DummySRng "Filler", typeAnnot = OkT}

testDescription :: TestTree
testDescription = testGroup  "Test the parseDescription"
    [testCase "parse gives the Descrption" $
        parseDescription  "{Player} participates in {Game}" @?= Descr "participates in" ["Player", "Game"]
    ,
    testCase "parse with no arguments" $
        parseDescription  "I am super high" @?= Descr "I am super high" []
    ]

testFlipDescription :: TestTree
testFlipDescription = testGroup "test checkNFlip"
   [testCase "flip binary pred that has wrong order" $
        checkNFlip lexicon1 participate_flipped  @?= participate_default
    ,
    testCase "don't flip binary pred that has right order" $
        checkNFlip lexicon1 participate_default @?= participate_default
    ,
    testCase "don't change unary predicate" $
        checkNFlip lexicon1 unary_predicate_shouldnt_flip @?= unary_predicate_shouldnt_flip
    ,
    testCase "don't flip with wrong lexicon" $
        checkNFlip lexicon2 participate_flipped @?= participate_flipped
    ,
    testCase "don't flip when names don't match" $
        checkNFlip lexicon2 participate_flipped_wrong_name @?= participate_flipped_wrong_name
   ]
   where
       lexicon1 :: Mapping ()
       lexicon1 = Mapping () "participate" (Descr "participates in" ["Player", "Game"])

       lexicon2 :: Mapping ()
       lexicon2 = Mapping () "participate" (Descr "participates in" ["Foo", "Bar"])

       participate_default :: VarDecl ()
       participate_default = Fun2 "participate" "Player" "Game"

       participate_flipped  :: VarDecl ()
       participate_flipped = Fun2 "participate" "Game" "Player"

       participate_flipped_wrong_name  :: VarDecl ()
       participate_flipped_wrong_name = Fun2 "FOO" "Game" "Player"

       unary_predicate_shouldnt_flip :: VarDecl ()
       unary_predicate_shouldnt_flip = VarDecl () [] (Arg1 "Player")
