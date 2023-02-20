{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Test.Tasty
import ExpSysTest ( esGraphUTs, esRuleUTs )

main :: IO ()
main = do
  defaultMain $ testGroup "Tests"
    [ esGraphUTs -- from ExpSysTest 
    , esRuleUTs  -- from ExpSysTest 
    ]


