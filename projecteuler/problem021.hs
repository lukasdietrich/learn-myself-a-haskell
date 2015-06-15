divisors :: Int -> [Int]
divisors n = [ d | d <- [1..limit], rem n d == 0]
    where limit = div n 2

d :: Int -> Int
d n = sum $ divisors n

main :: IO ()
main = print $ sum [ x | x <- [1..10000], d (d x) == x ]