{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 0
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = a == x || elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = 2 * x : doubleAll xs
 
squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = x ^ 2 : squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs) = if even x 
                    then x:selectEven xs 
                    else selectEven xs

average :: [Float] -> Float
average [] = 0
average x = sum' x /fromIntegral (length x)

geoAverage :: [Float] -> Float
geoAverage [] = 0
geoAverage x = product(x) ** (1 / fromIntegral (length x))

averages :: [Float] -> (Float, Float)
averages [] = (0, 0)
averages x = (sum' x /fromIntegral (length x), product(x) ** (1 / fromIntegral (length x)))

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
            where loop acc []     = acc
                loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
        where loop acc []     = acc
            loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
        where loop acc [] = acc
              loop acc (x:xs) = loop (acc * x) xs

length'2 :: [a] -> Int
length'2 = loop 0
 where loop acc [] = acc
       loop acc (x:xs) = loop (acc + 1) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
        where loop !acc []     = acc
                loop !acc (x:xs) = loop (x + acc) xs
