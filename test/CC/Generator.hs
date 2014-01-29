{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses
  #-}

module CC.Generator where

import Test.SmallCheck
import Test.SmallCheck.Series

import CC.Language
import CC.Tag

--
-- * Generators
--

genOpts :: Int -> [Option]
genOpts n = take n [c:"" | c <- ['A'..'Z']]

instance Monad m => Serial m Dim where
  series = generate (map Dim . genOpts)

instance Monad m => Serial m Formula where
  series = generate (map Opt . genOpts)
        \/ cons1 Con
        \/ cons1 Not
        \/ cons2 And
        \/ cons2 Or
