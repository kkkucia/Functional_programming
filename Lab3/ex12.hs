import Data.Char
import Data.List ()

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x : xs) = toUpper x : (map toLower xs)

formatStr :: String -> [Char]
formatStr s =
  foldr1 (\w s -> w ++ " " ++ s)
    . map capitalize
    . filter (\x -> length x > 1)
    $ words s

prodPrices :: Num a => String -> a
prodPrices p = case p of
  "A" -> 100
  "B" -> 500
  "C" -> 1000
  _ -> error "Unknown product"

products :: [String]
products = ["A", "B", "C"]

-- basic discount strategy
discStr1 :: (Ord a, Fractional a) => String -> a
discStr1 p
  | price > 999 = 0.3 * price
  | otherwise = 0.1 * price
  where
    price = prodPrices p

-- flat discount strategy
discStr2 :: Fractional a => String -> a
discStr2 p = 0.2 * prodPrices p

totalDiscout :: Num c => (String -> c) -> [String] -> c
totalDiscout discStr =
  foldl1 (+)
    . map discStr
    . filter (\p -> prodPrices p > 499)
    