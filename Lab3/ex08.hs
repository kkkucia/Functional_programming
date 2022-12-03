import Data.Char

doubleElems :: Num a => [a] -> [a]
doubleElems[]    = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems :: Num a => [a] -> [a]
sqrElems[]    = []
sqrElems (x:xs) = x * x : sqrElems xs


lowerCase :: [Char] -> [Char]
lowerCase[] = []
lowerCase (x:xs) = toLower x  : lowerCase xs


map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

doubleElems2 :: [Integer] -> [Integer]
doubleElems2 = map' (*2)

sqrElems2 :: [Double] -> [Double]
sqrElems2    = map' (\x -> sqrt(x))

lowerCase2 :: [Char] -> [Char]
lowerCase2   = map' toLower
