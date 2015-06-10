isPalindrom :: [Char] -> Bool
isPalindrom s = s == reverse s

findPalindrom :: [(Int, Int, Int)]
findPalindrom = 
    [(x, y, x * y) | x <- range, y <- range, isPalindrom $ show $ x * y]
    where range = [100..999]

betterPalindrom :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
betterPalindrom a@(_, _, b) c@(_, _, d)
    | b > d     = a
    | otherwise = c

main :: IO ()
main = print $ foldl1 betterPalindrom findPalindrom