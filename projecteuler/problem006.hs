squareSum :: [Int] -> Int
squareSum ns = sum $ map (^ 2) ns

sumSquare :: [Int] -> Int
sumSquare ns = (sum ns) ^ 2

diffSquareSum :: [Int] -> Int
diffSquareSum ns = (sumSquare ns) - (squareSum ns)

main :: IO ()
main = print $ diffSquareSum [1..100]