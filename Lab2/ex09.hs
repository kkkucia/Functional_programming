qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]

qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter ( <= x) (xs)
   rightPart xs = filter ( > x) (xs)


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y     = x:merge xs (y:ys)
    | otherwise  = y:merge (x:xs) ys

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort (leftPart xs)) (mSort (rightPart xs))
    where
        leftPart xs = take ((length xs + 1) `div` 2) xs
        rightPart xs = drop ((length xs + 1) `div` 2) xs

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n < x = n : x : xs
                | otherwise = x : insert n xs

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x <= head xs && isSorted xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [] [_] = []
zip' [_] [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
 
unzip' :: [(a, b)] -> ([a],[b]) 
unzip' [] = ([], [])
unzip'  ((x,y):zs) = let (xs,ys) = unzip zs 
                     in (x:xs, y:ys)

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] [] [] = []
zip3' [_] [] [] = []
zip3' [] [_] [] = []
zip3' [] [] [_] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList [_] []    = False
subList [] [_]    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys
