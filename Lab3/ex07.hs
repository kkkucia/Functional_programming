import Data.Char

onlyEven :: Integral a => [a] -> [a]
onlyEven [] = []
onlyEven (x : xs)
  | x `mod` 2 == 0 = x : onlyEven xs
  | otherwise = onlyEven xs

onlyOdd :: Integral a => [a] -> [a]
onlyOdd [] = []
onlyOdd (x : xs) =
  if x `mod` 2 == 1
    then x : onlyOdd xs
    else onlyOdd xs

onlyUpper :: [Char] -> [Char]
onlyUpper "" = ""
onlyUpper (x : xs) = if isUpper x then x : onlyUpper xs else onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) = if p x then x : filter' p xs else filter' p xs

onlyEven2 :: [Integer] -> [Integer]
onlyEven2 = filter' (\x -> x `mod` 2 == 1)

onlyOdd2 :: [Integer] -> [Integer]
onlyOdd2 = filter' (\x -> x `mod` 2 == 0)

onlyUpper2 :: [Char] -> [Char]
onlyUpper2 = filter' isUpper

length2 :: Integral a => [a] -> [a]
length2 xs = [x | x <- xs, x `mod` 2 == 0]
