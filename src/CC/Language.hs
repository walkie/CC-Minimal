
module CC.Language where

import Data.List (transpose)

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
  mapCC    :: (CC t e -> a) -> e (CC t e) -> e a
  foldCC   :: (CC t e -> a -> a) -> a -> e (CC t e) -> a
  showObj  :: Tag t => e (CC t e) -> String

class Tag t where
  -- | The configuration options referenced in this tag.
  tagOpts :: t -> Set Option
  -- | Resolve or simplify a tag given a configuration.
  resolve :: Config -> t -> Either t Bool
  -- | Pretty print a tag.
  showTag :: t -> String

showCC :: (Tag t, Obj e) => CC t e -> String
showCC (Obj e)     = showObj e
showCC (Chc t l r) = showTag t ++ "<" ++ showCC l ++ "," ++ showCC r ++ ">"

instance (Tag t, Obj e) => Show (CC t e) where
  show = showCC


--
-- * Semantics
--

-- | Configure option.
type Option = String

-- | Configuration option setting: on or off.
type Setting = (Option, Bool)

-- | (Potentially partial) configuration.
type Config = Map Option Bool

-- | Plain types correspond to the fixed-point of the object language
--   type constructor.
newtype Plain f = P { unP :: f (Plain f) }

-- | Denotational semantics.
type Semantics e = Map Config (Plain e)

-- | The set of all configuration options referred to in an expression.
options :: (Tag t, Obj e) => CC t e -> Set Option
options (Obj e) = foldCC (Set.union . options) Set.empty e
options (Chc t l r) = Set.unions [tagOpts t, options l, options r]

-- | All configurations of an expression.
configs :: (Tag t, Obj e) => CC t e -> [Config]
configs e = (map Map.fromList . transpose) [[(o,True),(o,False)] | o <- os]
  where os = Set.toList (options e)

-- | Apply a (potentially partial) configuration to an expression.
configure :: (Tag t, Obj e) => Config -> CC t e -> CC t e
configure c (Obj e)     = Obj (mapCC (configure c) e)
configure c (Chc t l r) =
    case resolve c t of
      Left t' -> Chc t' l' r'
      Right b -> if b then l' else r'
  where l' = configure c l
        r' = configure c r

-- | Convert an expression without choices into a plain expression.
toPlain :: (Tag t, Obj e) => CC t e -> Plain e
toPlain (Obj e) = P (mapCC toPlain e)
toPlain e       = error $ "toPlain: not plain: " ++ show e

-- | Compute the denotational semantics.
semantics :: (Tag t, Obj e) => CC t e -> Semantics e
semantics e = Map.fromList [(c, toPlain (configure c e)) | c <- configs e]
