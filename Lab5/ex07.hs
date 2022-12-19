newtype Box a = MkBox a deriving (Show)

instance Applicative Box where
  pure :: a -> Box a
  pure = MkBox
  (<*>) :: Box (a -> b) -> Box a -> Box b
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b
  fmap f (MkBox x) = MkBox (f x)
