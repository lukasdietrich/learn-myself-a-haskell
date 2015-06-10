import Data.List
import Data.Bool

-- creates a list that contains the elements of a list at n positions
accumulateSublist :: (Ord a) => [a] -> [Int] -> [a]
accumulateSublist l i = accumulateSublist' (sort l) i 1

accumulateSublist' :: [a] -> [Int] -> Int -> [a]
accumulateSublist' [] _ _ = [] 
accumulateSublist' _ [] _ = []
accumulateSublist' (l:ls) all@(i:is) c
    | c == i    = l:(accumulateSublist' ls is d)
    | otherwise = accumulateSublist' ls all d
    where d = succ c

-- determines if a game is over
winnerOfBoard :: [Char] -> Char -> Bool
winnerOfBoard b p =
    any (== True) (map ((all (== p)) . (accumulateSublist b)) rows)
    where rows = [[1,2,3], [4,5,6], 
                  [7,8,9], [1,4,7], 
                  [2,5,8], [3,6,9], 
                  [1,5,9], [3,5,7]]

-- handles the board on given input
changeBoard :: [Char] -> Char -> Int -> (Bool, [Char])
changeBoard all@(b:bs) p 1 = 
    if elem b ['x','o'] then (False, all) else (True, p:bs)
changeBoard (b:bs) p t = 
    (fst next, b:(snd next))
    where next = changeBoard bs p $ pred t

-- print the board 
printBoard :: [Char] -> IO ()
printBoard []    = return ()
printBoard board = do
    let split = splitAt 3 board
    putStrLn $ intersperse ' ' . intersperse '|' $ fst split
    printBoard $ snd split

-- alternates between b and c on given a
alternate :: (Eq a) => a -> a -> a -> a
alternate a b c
    | a == b    = c
    | otherwise = b

-- game loop
-- render -> is done? [-> change => repeat] 
loop :: Char -> [Char] -> IO ()
loop player board = do
    let other  = alternate player 'x' 'o'
    let winner = winnerOfBoard board other    
    
    printBoard board
    if winner
        then do
            putStrLn $ "Winner is " ++ [other]
        else do
            putStrLn $ "Do your turn '" ++ [player] ++ "' !"
            input <- readLn
            
            let next = changeBoard board player input
            loop (if fst next then other else player) $ snd next

main :: IO ()
main =
   loop 'x' ['1'..'9']
