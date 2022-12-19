{-# LANGUAGE DeriveFunctor #-}

-- newtype Box a = MkBox a deriving Show
-- newtype Box a = MkBox a deriving (Show, Functor)

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b
  fmap f (MkBox x) = MkBox (f x)

data MyList a
  = EmptyList
  | Cons a (MyList a)
  deriving (Show)

instance Functor MyList where
  fmap _ EmptyList = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

instance Functor (Arr e1 e2) where
  fmap g (Arr a) = Arr ((g .) . a)