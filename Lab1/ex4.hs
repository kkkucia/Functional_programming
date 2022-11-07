sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x ^ 2 + y ^ 2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = 
    let a = x ^ 2 + y ^ 2 + z ^ 2
    in sqrt a

swap :: (Int, Char) -> (Char, Int)
swap (x, y) = (y, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual(x, y, z) = (x == y) && (y == z)

heronTriangle :: (Double, Double, Double) -> Double
heronTriangle(a, b, c) = sqrt(0.5 * (a + b + c) * ((0.5 * (a + b + c)) - a) * ((0.5 * (a + b + c)) - b) * ((0.5 * (a + b + c)) - c))