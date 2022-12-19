test :: p -> IO ()
test s = getLine >>= \s -> return 3 >>= \n -> putStrLn $ show n ++ s

data Tree a
  = Node a (Tree a) (Tree a)
  | Leaf

pathsSum :: Num a => Tree a -> [a]
pathsSum Leaf = pure 0
pathsSum (Node a lt rt) = concat $ ([(a +)] <*>) <$> (fmap pathsSum [lt, rt])

paths :: Tree a -> [[a]]
paths Leaf = pure []
paths (Node a lt rt) = concat $ ([(a :)] <*>) <$> (fmap paths [lt, rt])