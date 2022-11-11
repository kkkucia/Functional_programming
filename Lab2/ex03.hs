import Data.List
let xs = 1 : 2 : 3 : 4 : 5 : []

ghci> xs
[1,2,3,4,5]

ghci> let xs = [1..5]
[1,2,3,4,5]

ghci> length xs
5

ghci> reverse xs
[5,4,3,2,1]

ghci> head xs
1

ghci> tail xs
[2,3,4,5]

ghci> last xs
5

ghci> init xs
[1,2,3,4]

ghci> 0 : xs
[0,1,2,3,4,5]

ghci> xs ++ [6]
[1,2,3,4,5,6]

ghci> xs !! 2
3

ghci> [1,2] ++ [3,4,5]
[1,2,3,4,5]

ghci> take 2 xs
[1,2]

ghci> drop 2 xs
[3,4,5]

ghci> null xs
False

ghci> any (> 2) xs
True

ghci> all (> 0) xs
True

ghci> zip xs ['a','b']
[(1,'a'),(2,'b')]

ghci> splitAt 2 xs
([1,2],[3,4,5])

ghci> sort [2,3,1,4,5]
[1,2,3,4,5]

ghci>  2 `elem` xs
True

ghci>  52 `elem` xs
False

ghci> minimum xs
1

ghci> maximum xs
5

ghci> sum xs
15

ghci> product xs
120
