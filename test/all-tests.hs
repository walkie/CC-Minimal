
module Main where

import Test.Tasty

import CC.LanguageTest
import CC.ObjectTest
import CC.ParserTest

main = defaultMain $
  testGroup "CC-Minimal" [
    CC.LanguageTest.tests,
    CC.ObjectTest.tests,
    CC.ParserTest.tests
  ]
