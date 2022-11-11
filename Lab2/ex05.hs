-- Napisać wyrażenie obliczające, ile jest w przedziale [1,100] trójek liczb całkowitych reprezentujących długości boków trójkąta prostokątnego.
--length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2,  a + b > c]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == [] -- 1 i 0 zwraca jako liczyby pierwsze

isPrimeBetter :: Integral t => t -> Bool
isPrimeBetter n = (n > 1) && null [x | x <- [2 .. sqrt n], n `mod` x == 0]

-- Napisać wyrażenie obliczające, ile jest w przedziale [1,10000] liczb pierwszych
-- length [n | n <- [2..1000], length [x | x <- [2..n-1], n `mod` x == 0] == 0]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime2 :: Int -> Bool
isPrime2 n = length [primes !! el | el <-[0..n], primes !! el == n] == 1

-- Napisać funkcje, która podaje, ile jest liczb pierwszych w przedziale [1,n]
primesToN :: Int -> Int
primesToN n = length [x | x <-[1..n], isPrimeBetter x]

allEqual :: Eq a => [a] -> Bool
allEqual xs = length xs == length [head xs | x <-[0..length xs - 1], xs !! x == head xs]
