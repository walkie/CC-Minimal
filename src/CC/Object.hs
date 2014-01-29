
module CC.Object where

import CC.Language

--
-- * Some Simple Object Languages
--

-- ** Empty/unit values

data None e = None
  deriving Eq

instance Obj None where
  mapCC   _ _ = None
  foldCC  _ b = const b
  showObj   _ = "_"


-- ** Atomic values

data One a e = One a
  deriving Eq

instance Show a => Obj (One a) where
  mapCC   _ (One a) = One a
  foldCC  _ b       = const b
  showObj   (One a) = show a


-- ** Lists

data List a e =
    Nil
  | Cons a e
  deriving Eq

instance Show a => Obj (List a) where
  
  mapCC f (Cons a e) = Cons a (f e)
  mapCC _ Nil        = Nil
  
  foldCC _ b Nil        = b
  foldCC f b (Cons _ e) = f e b
  
  showObj Nil        = "[]"
  showObj (Cons a e) = show a ++ ":" ++ show e


-- ** Binary trees

data Tree a e =
    Leaf a
  | Node a e e
  deriving Eq

instance Show a => Obj (Tree a) where
  
  mapCC f (Node a l r) = Node a (f l) (f r)
  mapCC _ (Leaf a)     = Leaf a
  
  foldCC _ b (Leaf _)     = b
  foldCC f b (Node _ l r) = f r (f l b)
  
  showObj (Leaf a)     = show a
  showObj (Node a l r) = unwords ["(Node", show a, show l, show r ++ ")"]


--
-- * Plain Types
--

type None'   = Plain None
type One'  a = Plain (One a)
type List' a = Plain (List a)
type Tree' a = Plain (Tree a)

-- ** Smart constructors

none :: None'
none = P None

one :: a -> One' a
one = P . One

nil :: List' a
nil = P Nil

cons :: a -> List' a -> List' a
cons a l = P (Cons a l)

fromList :: [a] -> List' a
fromList = foldr cons nil

toList :: List' a -> [a]
toList (P Nil)        = []
toList (P (Cons a e)) = a : toList e

leaf :: a -> Tree' a
leaf = P . Leaf

node :: a -> Tree' a -> Tree' a -> Tree' a
node a l r = P (Node a l r)
