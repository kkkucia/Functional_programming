sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs


sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x*x + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f(x) + sumWith f xs


sum'2 :: [Integer] -> Integer
sum'2 = sumWith (\x -> x)

sumSqr :: [Double] -> Double
sumSqr = sumWith (\x -> x^2)

sumCube :: [Double] -> Double
sumCube = sumWith (\x -> x^3)

sumAbs :: [Integer] -> Integer
sumAbs = sumWith (\x -> if x>0 then x else -x)

-- ∑15i=i1 i^5 in ghci ?
-- sumWith (\x -> x^5) [1..15]

listLength :: Num a => [a] -> a
listLength = sumWith (\x -> 1)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f(x) *  prodWith f xs

prod2 :: [Integer] -> Integer
prod2 = prodWith (\x -> x)

prodSqr :: [Integer] -> Integer
prodSqr = prodWith (\x -> x^2)

prodCube :: [Integer] -> Integer
prodCube = prodWith (\x -> x^3)

prodAbs :: [Integer] -> Integer
prodAbs = prodWith (\x -> abs(x))
