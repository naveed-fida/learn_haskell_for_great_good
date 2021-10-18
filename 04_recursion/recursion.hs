maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Can't find maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _           = []
zip' _ []           = []
zip' (x:xs) (y:ys)  = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y    = True
    | otherwise = elem' x ys

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    qsort smallerOrEqual ++ [x] ++ qsort larger
    where smallerOrEqual = [a | a <- xs, a <= x]
          larger         = [a | a <- xs, a > x]