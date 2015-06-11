isEvenlyDivisable :: Int -> [Int] -> Bool
isEvenlyDivisable _ []      = True
isEvenlyDivisable i (x:xs)  = mod i x == 0 && isEvenlyDivisable i xs

filterDivisor :: [Int] -> [Int]
filterDivisor []     = []
filterDivisor (x:xs) = x:(filterDivisor [y | y <- xs, mod x y /= 0])

findDivisable :: [Int] -> [Int] -> Int
findDivisable (x:xs) d
    | isEvenlyDivisable x d = x
    | otherwise             = findDivisable xs d

main :: IO ()
main = print $ findDivisable [20,40..] $ filterDivisor [20,19..1]