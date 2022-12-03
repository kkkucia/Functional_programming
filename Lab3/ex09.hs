sumWith :: Num a => (t -> a) -> [t] -> a
sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith :: Num a => (t -> a) -> [t] -> a
prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x $ foldr' f z xs

sumWith'' :: Num a => (t -> a) -> [t] -> a
sumWith'' g = foldr' (\x acc -> g x + acc) 0

prodWith'' :: Num a => (t -> a) -> [t] -> a
prodWith'' g = foldr' (\x prod -> g x * prod) 1
