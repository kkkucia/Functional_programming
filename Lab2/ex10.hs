fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstDivSnd :: Integral a => [a] -> Bool
fstDivSnd (x : y : _) | y `mod` x == 0 = True
fstDivSnd _                            = False

fstDivThrd:: Integral a => [a] -> Bool
fstDivThrd (x : y : z : _) | z `mod` x == 0 = True
fstDivThrd _                            = False
