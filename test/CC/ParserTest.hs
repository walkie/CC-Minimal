{-# LANGUAGE TemplateHaskell #-}

module CC.ParserTest where

import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.TH

import CC.Language
import CC.Tag
import CC.Parser
import CC.Generator


prop_showParseFormula = forAll showParse
  where
    showParse :: Formula -> Bool
    showParse f = f == runParser parseIt (showTag f)

tests = $(testGroupGenerator)
