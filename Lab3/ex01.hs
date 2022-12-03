-- f1(x)=x−2; x∈ℝ
f1 :: Integer -> Integer
f1 = \x -> x - 2

-- f2(x,y)=(√x2+y2); x,y∈ℝ
f2 :: (Double, Double) -> Double
f2 = \x y -> sqrt (x ^ 2 + y ^ 2)

-- f3(x,y,z)=(√x2+y2+z2); x,y,z∈ℤ
f3 :: (Double, Double, Double) -> Double
f3 = \x y z -> sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- (2*), (*2), (2^), (^2), (2/), (/3), (4-) - (lambda function)
h1 :: Integer -> Integer
h1 = \x -> 2 * x

h2 :: Integer -> Integer
h2 = \x -> x * 2

h3 :: Double -> Double
h3 = \x -> x ** 2

h4 :: Double -> Double
h4 = \x -> 2 ** x

h5 :: Double -> Double
h5 = \x -> 2 / x

h6 :: Double -> Double
h6 = \x -> x / 3

h7 :: Integer -> Integer
h7 = \x -> 4 - x

-- f7 x = if x `mod` 2 == 0 then True else False
f7 :: Integer -> Bool
f7 = \x -> x `mod` 2 == 0

-- f8 x = let y = sqrt x in 2 * y^3 * (y + 1)
f8 :: Double -> Double
f8 = \x -> 2 * (sqrt x) ** 3 * ((sqrt x) + 1)

-- f9 1 = 3
-- f9 _ = 0
f9 :: Integer -> Integer
f9 = \x -> if x == 1 then 3 else 0
