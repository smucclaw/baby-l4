{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import DisambiguateTest
import ToGFTest



main :: IO ()
main = do
  pgf <- DisambiguateTest.getPgf
  defaultMain $ testGroup "Tests"
    [ disambiguateTests pgf -- from DisambiguateTest
    , testGetAtoms -- from ToGFTest
    , testDescription -- from ToGFTest
    ]


