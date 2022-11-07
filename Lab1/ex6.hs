absInt :: Int -> Int
absInt n | n > 0 = n
         | otherwise = -n

sgn :: Int -> Int
sgn n | n < 0 = -1
      | n == 0 = 0
      | otherwise = 1

min3Int :: (Int, Int, Int) -> Int 
min3Int (x, y, z) | x <= y && x <= z = x
                  | y <= x && y <= z = y
                  | otherwise = z
                    
toUpper :: Char -> Char
toUpper x | fromEnum x  >= 65 && fromEnum x <= 90 = x
          | otherwise = toEnum (fromEnum x - 32)

toLower :: Char -> Char
toLower x | fromEnum x  >= 97 && fromEnum x <= 122 = x
          | otherwise = toEnum (fromEnum x - 32)

isDigit :: Char -> Bool
isDigit x | fromEnum x <= 57 && fromEnum x >= 48 = True
          | otherwise = False

charToNum :: Char -> Int
charToNum x | not (isDigit x) = fromEnum x - 48
            | otherwise = 0

romanDigit :: Char -> String
romanDigit x
    | charToNum x == 1 = "I"
    | charToNum x == 2 = "II"
    | charToNum x == 3 = "III"
    | charToNum x == 4 = "IV"
    | charToNum x == 5 = "V"
    | charToNum x == 6 = "VI"
    | charToNum x == 7 = "VII"
    | charToNum x == 8 = "VIII"
    | charToNum x == 9 = "IX"
