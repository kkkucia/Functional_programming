sqr :: Num a => a -> a
sqr x = x ^ 2

funcFactory :: (Num a, Eq a) => a -> a -> a
funcFactory n = case n of
  1 -> id
  2 -> sqr
  3 -> (^ 3)
  4 -> \x -> x ^ 4
  5 -> intFunc
  _ -> const n
  where
    intFunc x = x ^ 5

expApproxUpTo :: (Enum a, Floating a) => a -> a -> a
expApproxUpTo n x = sum [func k x | k <- [0 .. n]]
  where
    func k x = x ** k / fac k
    fac n = product [1 .. n]
