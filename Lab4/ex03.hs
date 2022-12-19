data BinIntTree
  = EmptyIntBT
  | IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a
  = EmptyBT
  | NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a
  = Lit a -- literal/value a, e.g. Lit 2 = 2
  | Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n l r) = max (1 + depthOfBT r) (1 + depthOfBT l)

flattenBTPreorder :: BinTree a -> [a] -- napisać trzy wersje: preorder, inorder, postorder
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT n l r) = [n] ++ flattenBTPreorder l ++ flattenBTPreorder r

flattenBTInorder :: BinTree a -> [a]
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT n l r) = flattenBTInorder l ++ [n] ++ flattenBTInorder r

flattenBTPostorder :: BinTree a -> [a]
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT n l r) = flattenBTPostorder l ++ flattenBTPostorder r ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n l r) = NodeBT (f n) (mapBT f l) (mapBT f r)


insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert el EmptyBT = NodeBT el EmptyBT EmptyBT
insert el (NodeBT n l r)    | el > n = NodeBT n l (insert el r)
                            | el < n = NodeBT n (insert el l) r
                            | otherwise = NodeBT n l r


instance Eq a => Eq (BinTree a) where
    (==) :: Eq a => BinTree a -> BinTree a -> Bool
    (==) (NodeBT n l r) (NodeBT n2 l2 r2) = n==n2 && l==l2  && r==r2
    (==) (EmptyBT) (EmptyBT) = True
