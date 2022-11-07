absInt :: Int -> Int
absInt n = case (n >= 0) of
            True -> n
            _ -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer n = case n of
            "Answer" -> True
            _ -> False

not' :: Bool -> Bool
not' x = case x of
        True -> False
        False -> True 

or' :: (Bool, Bool) -> Bool
or' x = case x of
    (False, False) -> False
    _ -> True

and' :: (Bool, Bool) -> Bool
and' x = case x of
    (True, True) -> True
    _-> False

nand' :: (Bool, Bool) -> Bool
nand' x = case x of
          (True, True) -> False
          _ -> True

xor' :: (Bool, Bool) -> Bool
xor' x = case x of
         (True, True) -> False
         (False, False) -> False
         _ -> True
