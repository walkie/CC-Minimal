
module CC.ObjectTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import CC.Object

prop_fromToList = forAll (\l -> l == toList (fromList (l :: [Int])))

tests = testGroup "CC.Object" [
    testProperty "fromToList" prop_fromToList
  ]
