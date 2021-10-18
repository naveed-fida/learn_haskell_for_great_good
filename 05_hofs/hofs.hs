-- Conventional way to write a function that compares a fixed value, in this case 100,
    -- with a provided value
compareWithHundred :: Integer -> Ordering
compareWithHundred x = compare 100 x

-- Taking advantage of currying, and partial application
compareWithHundred' :: Integer -> Ordering
compareWithHundred' = compare 100

-- Infix functions can also be partially applied using sections. Not that the partiall application
    -- be done to either argument
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

-- Another example
isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

-- Watch out when using subtraction. `(- 9) means negative 9. Use `subtract` instad
subtractTwo :: (Num a) => a -> a
subtractTwo = (2 `subtract`)

-- A function that takes a function and applies it twice. Note the type declaration
applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

-- An implementation of the built-in `zipWith`. Takes a function and two lists and applies
    -- the function to successive pairs with one elem drawn from first list and other from second
    -- returns a list of the given function's return values
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = [] 
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Usage examples:
    {-
    ```
    ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
    ghci> zipWith' max [6,3,2,1] [7,3,1,5]
    ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
    ghci> zipWith' zipWith' (*) (replicate 5 2) [1..]
    ```
    -}

-- An instructive example of the use of `zipWith`. This uses a partially applied function as
    -- the first argument. A fine example of HOFs in conjunction with partial application.
    {-
    ```
    ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
    ```
    -}

-- An implementation of the standard library `flip`. Takes a function and returns the same
    -- function with arguments flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- Another way to write flip'. Takes advantage of partial application
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

-- A `map` implementation.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- `map` usage. All these examples can be rewritten using a list comprehension
{-
```
ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
```
-}

-- An implementation of `filter`. We're using the variable p for the function to imply that
    -- the function is a predicate, i.e., a function that checks a value against some condition 
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter p xs

-- Usage examples. Again all these can be rewritten using a comprehension
{-
```
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
ghci> filter (==3) [1,2,3,4,5]
[3]
ghci> filter even [1..10]
[2,4,6,8,10]
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
[[1,2,3],[3,4,5],[2,2]]
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"
ghci> filter (`elem` ['A'..'Z']) "i LAuGh at you bEcause u R all the same"
"LAGER"
```
-}

-- An example that applies several predicates. The second example is note-worthy since
    -- it uses a lambda (anonymous function) to join the two predicates into one
{-
```
ghci> filter (<15) (filter even [1..20])
[2,4,6,8,10,12,14]

ghci > filter (\x -> even x && x < 15) [1..20]
[2,4,6,8,10,12,14]
```
-}

-- A version of `quicksort` that uses filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- find the largest num- ber under 100,000 that’s divisible by 3,829. Note that haskell's laziness
    -- means that this is quite an efficient solution and the evaluation is stopped as soon as
    -- there is a `head` meaning the first solution.
largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0