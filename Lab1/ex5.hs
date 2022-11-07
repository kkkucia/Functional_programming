sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n >= 0 then n else -n

min2Int :: (Int, Int) -> Int 
min2Int(x, y) = if x <= y then x else y

min3Int :: (Int, Int, Int) -> Int 
min3Int(x, y, z) = if x <= y && x <= z 
                    then x
                    else if y <= x && y <= z 
                    then y
                    else z

min3IntNew :: (Int, Int, Int) -> Int 
min3IntNew(x, y, z) = min2Int (x, min2Int (y, z))

toUpper :: Char -> Char
toUpper x = if fromEnum x  >= 97 && fromEnum x <= 122
            then toEnum (fromEnum x - 32)
            else x

toLower :: Char -> Char
toLower x = if fromEnum x  >= 65 && fromEnum x <= 90 
            then toEnum (fromEnum x + 32)
            else x

isDigit :: Char -> Bool
isDigit x = fromEnum x <= 57 && fromEnum x >= 48 

charToNum :: Char -> Int
charToNum x = if not (isDigit x) then fromEnum x - 48 else 0

romanDigit :: Char -> String
romanDigit x = if x == '1' then "I"
                else if x == '2' then "II"
                    else if x == '3' then "III"
                        else if x == '4' then "IV"
                            else if x == '5' then "V"
                                else if x == '6' then "VI"
                                    else if x == '7' then "VII"
                                        else if x == '8' then "VIII"
                                            else "IX"
