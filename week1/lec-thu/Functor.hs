-- Functor.hs
--
-- class Functor f :: Type -> Type where
--   fmap :: (a -> b) -> f a -> f b
--
-- 1. fmap id x == id x
--
-- 2. fmap (f . g) == fmap f . fmap g

data Box a = Box a
  deriving (Show, Eq)

instance Functor Box where
  -- We have:
  -- f :: a -> b
  -- x :: a
  -- y :: b
  -- Want:
  -- Box b
  fmap f (Box x) =
    let y = f x
    in Box y

-- fmap id (Box x)
-- = Box (id x)
-- = id (Box x)

data Fun t1 t2 = Fun (t1 -> t2)

instance Functor (Fun a) where
  -- fmap :: (a -> b) -> Fun t1 a -> Fun t1 b
  -- We have:
  -- f :: a -> b
  -- g :: t1 -> a
  -- h :: t1 -> b
  -- Want:
  -- Fun t1 b
  fmap f (Fun g) =
    let h = f . g
    in Fun h

data Opt a = Some a | None
  deriving (Show)

instance Functor Opt where
  -- f :: a -> b
  -- x :: Opt a
  -- Want: Opt b
  fmap f x =
    case x of
      Some x' ->
        -- x' :: a
        -- y :: b
        let y = f x'
        in Some y
      None -> None

optHead :: [a] -> Opt a
optHead [] = None
optHead (x:_) = Some x

-- f <$> x = fmap f x
