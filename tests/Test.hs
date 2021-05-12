{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import DisambiguateTest

main :: IO ()
main = do
  pgf <- DisambiguateTest.getPgf
  defaultMain $ testGroup "Tests"
    [ disambiguateTests pgf
    ]


