-- Seq.hs
module Seq (Seq,
           isEmpty,
           singleton) where

data Seq a =
    Empty
  | Single a
  | Concat (Seq a) (Seq a)
  deriving (Show)

-- Nothing on empty sequences
uncons :: Seq a -> Maybe (a, Seq a)
uncons = undefined

instance Eq a => Eq (Seq a) where
  s1 == s2 =
    case (uncons s1, uncons s2) of
      (Nothing, Nothing) -> True
      (Just (x, s1'), Just (y, s2')) ->
        x == y && s1' == s2'
      _ -> False

isEmpty :: Seq a -> Bool
isEmpty Empty = True
isEmpty (Concat l1 l2) = isEmpty l1 && isEmpty l2
isEmpty (Single _) = False

singleton :: a -> Seq a
singleton = Single

instance Functor Seq where
  -- f: a -> b
  -- l: Seq a
  -- Want: Seq b
  fmap f l =
    case l of
      Empty ->
        Empty
      Single x ->
        -- x: a
        Single (f x)
      Concat x y ->
        -- x: Seq a
        -- y: Seq a
        -- x' : Seq b
        -- y' : Seq b
        let x' = fmap f x
            y' = fmap f y
        in Concat x' y'

instance Semigroup (Seq a) where
  Empty <> s2 = s2
  s1 <> Empty = s1
  s1 <> s2 = Concat s1 s2

instance Monoid (Seq a) where
  mempty = Empty

instance Foldable Seq where
  -- foldMap :: Monoid m => (a -> m)
  --                     -> Seq a
  --                     -> m
  --
  -- Have:
  -- f: a -> m
  -- s: Seq a
  --
  -- Want: m
  foldMap f s =
    case s of
      Empty -> mempty
      Single x ->
        -- x: a
        f x
      Concat s1 s2 ->
        -- s1: Seq a
        -- s2: Seq a
        -- s1': m
        -- s2': m
        let s1' = foldMap f s1
            s2' = foldMap f s2
        in s1' <> s2'

data Sum a = Sum a

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x+y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

--length :: Foldable t => t a -> Int
--length l = let Sum x = foldMap (\_ -> Sum 1) l
--           in x

--sum :: (Num a, Foldable t) => t a -> a
--sum l = let Sum x = foldMap (\x -> Sum x) l
--        in x

data Vec3 a = Vec3 a a a
  deriving (Show)

instance Foldable Vec3 where
  foldMap f (Vec3 x y z) =
    f x <> f y <> f z

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

normalize :: (Foldable t,
              Functor t,
              Floating num,
              Fractional num) =>
          t num -> t num
normalize v =
  let s = sum (fmap (**2) v)
  in fmap (\x -> x / s) v
