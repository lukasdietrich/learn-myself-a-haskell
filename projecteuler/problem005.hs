isEvenlyDivisable :: Int -> [Int] -> Bool
isEvenlyDivisable _ []      = True
isEvenlyDivisable i (x:xs)  = mod i x == 0 && isEvenlyDivisable i xs

findDivisable :: [Int] -> [Int] -> Int
findDivisable (x:xs) d
    | isEvenlyDivisable x d = x
    | otherwise             = findDivisable xs d

main :: IO ()
main = print $ findDivisable [20,40..] [1..20]