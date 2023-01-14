import Control.Monad (guard)

triples :: Integral c => c -> [(c, c, c)]
triples n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], (a ^ 2 + b ^ 2) `mod` c ^ 2 == 1]

triples1 :: Integral c => c -> [(c, c, c)]
triples1 n = do
  let vals = [1 .. n]
  a <- vals
  b <- vals
  c <- vals
  guard $ (a ^ 2 + b ^ 2) `mod` c ^ 2 == 1
  return (a, b, c)

tryFactorial :: Int -> Maybe Int
tryFactorial 0 = Just 1
tryFactorial n =
  if n < 0
    then Nothing
    else do
      prev <- tryFactorial $ n - 1
      return $ n * prev