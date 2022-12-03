concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

-- list comprehension
concatList :: [[a]] -> [a]
concatList [] = []
concatList xs = [(xs!!i)!!j | i<-[0..(length xs-1)], j<-[0..(length (xs!!i)-1)]]

-- foldr
concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (++) []
