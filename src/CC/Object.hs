
module CC.Object where

import CC.Language

--
-- * Some Simple Object Languages
--

data One a b = One a
  deriving Eq

instance Show a => Obj (One a) where
  mapCC   _       = id
  foldCC  _ b     = const b
  showObj (One a) = show a

data None b = None
  deriving Eq

instance Obj None where
  mapCC   _   = id
  foldCC  _ b = const b
  showObj _   = "_"

data List a e =
    Nil
  | Cons a e
  deriving Eq

instance Show a => Obj (List a) where
  
  mapCC f (Cons a e) = Cons a (f e)
  mapCC _ n          = n
  
  foldCC _ b Nil        = b
  foldCC f b (Cons _ e) = f e b
  
  showObj Nil        = "[]"
  showObj (Cons a e) = show a ++ ":" ++ show e

data Tree a e =
    Leaf a
  | Node a e e
  deriving Eq

instance Show a => Obj (Tree a) where
  
  mapCC f (Node a l r) = Node a (f l) (f r)
  mapCC _ l            = l
  
  foldCC _ b (Leaf _)     = b
  foldCC f b (Node _ l r) = f r (f l b)
  
  showObj (Leaf a)     = show a
  showObj (Node a l r) = unwords ["(Node", show a, show l, show r ++ ")"]


--
-- * Plain Types
--

type None'   = Fix None
type One'  a = Fix (One a)
type List' a = Fix (List a)
type Tree' a = Fix (Tree a)

none :: None'
none = Fix None

one :: a -> One' a
one = Fix . One

nil :: List' a
nil = Fix Nil

cons :: a -> List' a -> List' a
cons a l = Fix (Cons a l)

leaf :: a -> Tree' a
leaf = Fix . Leaf

node :: a -> Tree' a -> Tree' a -> Tree' a
node a l r = Fix (Node a l r)
