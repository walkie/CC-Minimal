
module CC.Tag where

import Data.Bifunctor
import Data.Map (lookup)
import Data.Set (Set,empty,singleton,union)

import Prelude hiding (lookup)

import CC.Language


--
-- * Dimensions (atomic tags)
--

newtype Dim = Dim { name :: Option }
  deriving (Eq,Ord)

instance Show Dim where
  show = name

instance Tag Dim where
  tagOpts     = singleton . name
  resolve m d = maybe (Left d) Right (lookup (name d) m)
  showTag     = show


-- * Formula tags

-- | Boolean tag formulas.
data Formula = Con Bool
             | Opt Option
             | Not Formula
             | And Formula Formula
             | Or  Formula Formula
  deriving Eq

instance Show Formula where
  show f = case f of
      And f g -> paren f ++ "∧" ++ paren g
      Or  f g -> paren f ++ "∨" ++ paren g
      f       -> paren f
    where
      paren (Con b) = show b
      paren (Opt t) = t
      paren (Not f) = "¬" ++ paren f
      paren f       = "(" ++ show f ++ ")"

instance Tag Formula where
  
  tagOpts (Con _)   = empty
  tagOpts (Opt o)   = singleton o
  tagOpts (Not f)   = tagOpts f
  tagOpts (And l r) = tagOpts l `union` tagOpts r
  
  resolve m t@(Opt o) = maybe (Left t) Right (lookup o m)
  resolve m (Not f)   = bimap Not not (resolve m f)
  resolve m (And l r) = join And (&&) (resolve m l) (resolve m r)
  resolve m (Or  l r) = join Or  (||) (resolve m l) (resolve m r)
  
  showTag = show

-- | Helper function for 'resolve'.
join f g (Left  l) (Left  r) = Left  (f l r)
join f g (Left  l) (Right r) = Left  (f l (Con r))
join f g (Right l) (Left  r) = Left  (f (Con l) r)
join f g (Right l) (Right r) = Right (g l r)
