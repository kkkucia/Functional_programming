data Tree2 a = EmptyT2 | Leaf a | NodeT2 (Tree2 a) a (Tree2 a) deriving (Show)

instance Foldable Tree2 where
  foldMap :: Monoid m => (a -> m) -> Tree2 a -> m
  foldMap f EmptyT2 = mempty
  foldMap f (Leaf x) = f x
  foldMap f (NodeT2 l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Functor Tree2 where
  fmap :: (a -> b) -> Tree2 a -> Tree2 b
  fmap f EmptyT2 = EmptyT2
  fmap f (Leaf x) = Leaf $ f x
  fmap f (NodeT2 l x r) =
    NodeT2
      (fmap f l)
      (f x)
      (fmap f r)

instance Traversable Tree2 where
  traverse :: Applicative f => (a -> f b) -> Tree2 a -> f (Tree2 b)
  traverse f EmptyT2 = pure EmptyT2
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (NodeT2 l x r) =
    NodeT2
      <$> traverse f l
      <*> f x
      <*> traverse f r