fun :: String -> Int 
fun = sum . filter((> 2). length ). filtr(elem volwes) . words
    where vowels= "aeyiou"