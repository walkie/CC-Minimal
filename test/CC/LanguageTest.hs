
module CC.LanguageTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import CC.Language

tests = testGroup "CC.Language" [
    testCase "" (True @=? True)
  ]
