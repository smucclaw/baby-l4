{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import DisambiguateTest
import ToGFTest
import ExpSysTest ( esGraphUTs, esRuleUTs )

main :: IO ()
main = do
  pgf <- DisambiguateTest.getPgf
  defaultMain $ testGroup "Tests"
    [ disambiguateTests pgf -- from DisambiguateTest
    , testGetAtoms -- from ToGFTest
    , testDescription -- from ToGFTest
    , testFlipDescription -- from ToGFTest
    , esGraphUTs -- from ExpSysTest 
    , esRuleUTs  -- from ExpSysTest 
    ]


