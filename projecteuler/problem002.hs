findFibonacci :: Int -> [Int]
findFibonacci limit = 1:2:(findFibonacci' limit 1 2)

findFibonacci' :: Int -> Int -> Int -> [Int]
findFibonacci' limit a b
    | next < limit = next:(findFibonacci' limit b next)
    | otherwise    = []
    where next = a + b

main :: IO ()
main = print $ sum $ filter even $ findFibonacci 4000000