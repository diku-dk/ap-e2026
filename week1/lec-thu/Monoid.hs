-- Monoid.hs

-- type t
-- op :: t -> t -> t
-- n :: t
-- op x n == x == op n x
-- associative: (x `op` y) `op` z ==
--              x `op` (y `op` z)

-- consider integers
-- type = int
-- op = (+)
-- n = 0

-- class Semigroup a where
--   (<>) :: a -> a -> a

-- class (Semigroup) => Monoid a where
--   mempty :: a

-- x <> mempty == mempty <> x == x

data List a = Nil | Cons a (List a)

instance Semigroup (List a) where
  Nil <> l2 = l2
  Cons x xs <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil
