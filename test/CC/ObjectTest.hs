{-# LANGUAGE TemplateHaskell #-}

module CC.ObjectTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.Tasty.TH

import CC.Object

prop_fromToList = forAll fromToList
  where
    fromToList :: [Int] -> Bool
    fromToList l = l == toList (fromList l)

tests = $(testGroupGenerator)
