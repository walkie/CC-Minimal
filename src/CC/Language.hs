
module CC.Language where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


--
-- * Syntax
--

-- | Minimal, generic, binary choice calculus syntax.
data CC t e =
    Obj (e (CC t e))
  | Chc t (CC t e) (CC t e)

class Obj e where
  mapCC   :: (CC t e -> CC t e) -> e (CC t e) -> e (CC t e)
  foldCC  :: (CC t e -> b -> b) -> b -> e (CC t e) -> b
  showObj :: Show t => e (CC t e) -> String

instance (Show t, Obj e) => Show (CC t e) where
  show (Obj e)     = showObj e
  show (Chc t l r) = show t ++ "<" ++ show l ++ "," ++ show r ++ ">"


--
-- * Semantics
--

-- | Configure option.
type Option = String

-- | Configuration option setting: on or off.
type Setting = (Option, Bool)

-- | (Potentially partial) configuration.
type Config = Map Option Bool

-- | Plain types correspond to the fixed-point of the object language type.
newtype Fix f = Fix { unFix :: f (Fix f) }

-- | Denotational semantics.
type Semantics e = Map Config (Fix e)

class Tag t where
  tagOpts :: t -> Set Option
  resolve :: Config -> t -> Either t Bool

options :: (Tag t, Obj e) => CC t e -> Set Option
options (Obj e) = foldCC (Set.union . options) Set.empty e
options (Chc t l r) = Set.unions [tagOpts t, options l, options r]

partial :: (Tag t, Obj e) => Config -> CC t e -> CC t e
partial c (Obj e)     = Obj (mapCC (partial c) e)
partial c (Chc t l r) =
    case resolve c t of
      Left t' -> Chc t' l' r'
      Right b -> if b then l' else r'
  where l' = partial c l
        r' = partial c r

semantics :: (Tag t, Obj e) => CC t e -> Semantics e
semantics = undefined
