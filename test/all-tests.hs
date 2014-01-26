
module Main where

import Test.Tasty

import CC.LanguageTest

main = defaultMain $
  testGroup "CC-Minimal" [
    CC.LanguageTest.tests
  ]
