import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDescPF :: Ord a => [a] -> [a]
sortDescPF xs = reverse (sort xs)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g [] = True
are2FunsEqAt f g xs = f (head xs) == g (head xs) && are2FunsEqAt f g (drop 1 xs)

