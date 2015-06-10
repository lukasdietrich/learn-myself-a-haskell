isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

findPalindrome :: [(Int, Int, Int)]
findPalindrome = 
    [(x, y, x * y) | x <- range, y <- range, isPalindrome $ show $ x * y]
    where range = [100..999]

betterPalindrome :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
betterPalindrome a@(_, _, b) c@(_, _, d)
    | b > d     = a
    | otherwise = c

main :: IO ()
main = print $ foldl1 betterPalindrome findPalindrome