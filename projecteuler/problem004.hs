isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

findPalindrome :: [(Int, Int, Int)]
findPalindrome = 
    [(x, y, x * y) | x <- [100..999], y <- [x..999], isPalindrome $ show $ x * y]

betterPalindrome :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
betterPalindrome a@(_, _, b) c@(_, _, d)
    | b > d     = a
    | otherwise = c

main :: IO ()
main = print $ foldl1 betterPalindrome findPalindrome