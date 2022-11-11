import Data.Char (toUpper)

isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: [a] -> Int -> a
getElemAtIdx tab idx = head (drop idx tab)


capitalize :: [Char] -> [Char]
capitalize w =  toUpper (head w) : drop 1 w
