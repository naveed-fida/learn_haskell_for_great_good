-- Conventional way to write a function that compares a fixed value, in this case 100,
    -- with a provided value
compareWithHundred :: Integer -> Ordering
compareWithHundred x = compare 100 x

-- Taking advantage of currying, and partial application
compareWithHundred' :: Integer -> Ordering
compareWithHundred' = compare 100

-- Infix functions can also be partially applied using sections. Note that the partial application
    -- can be done to either argument
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

-- Another example
isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

-- Watch out when using subtraction. `(- 9)` means negative 9. Use `subtract` instead
subtractTwo :: (Num a) => a -> a
subtractTwo = (2 `subtract`)

-- A function that takes a function and applies it twice. Note the type declaration
    -- Note a nice property that currying gives us. Calling applyTwice with just the function
    -- as arugment gives you a function that you can call any time and it will be applied twice
    -- when you call it
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

-- find the largest number under 100,000 that’s divisible by 3,829. Note that haskell's laziness
    -- means that this is quite an efficient solution and the evaluation is stopped as soon as
    -- there is a `head`, meaning the first element satisfying the condition.
largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- With a lambda
largestDivisible' :: Integer
largestDivisible' = head (filter (\x -> x `mod` 3829 == 0) [100000,99999..])

-- Find the sum of all odd squares that are smaller than 10,000. Uses `takeWhile`. It takes elements
    -- from list as long as the predicate is true

oddSquares :: Integer
oddSquares = sum (takeWhile (<10000) ([s | s <- [x^2 | x <- [1..]], odd s]))

-- A version with `map`
oddSquares' :: Integer
oddSquares' = sum (filter odd (takeWhile (<10000) (map (^2) [1..])))

-- Tips: Don't use filter to take from infinite lists. Wont't work. Use `take` or `takeWhile`.
    -- Also, don't use `map` or `filter` when using comprehension to construct a list. Predicates
    -- inside the comprehension do that anyway.

{-
For our next problem, we’ll be dealing with Collatz sequences. A Collatz sequence (also known as a Collatz chain) is defined as follows:
• Start with any natural number.
• If the number is 1, stop.
• If the number is even, divide it by 2.
• If the number is odd, multiply it by 3 and add 1.
• Repeat the algorithm with the resulting number.

In essence, this gives us a chain of numbers. Mathematicians theorize that for all starting numbers,
the chain will finish at the number 1. For example, if we start with the number 13,
we get this sequence: 13, 40, 20, 10, 5,16,8,4,2,1.
We can see that the chain that starts with 13 has 10 terms.

For all starting numbers between 1 and 100, how many Collatz chains have a length greater than 15?
-}

collatzChain :: Int -> [Int]
collatzChain 1 = []
collatzChain x
    | even x    = x : collatzChain (x `div` 2)
    | otherwise = x : collatzChain (x * 3 + 1)

longChains :: Int
longChains = length [length xs | xs <- [collatzChain x | x <- [1..100]], length xs > 15]

-- with `map`, `filter` and a lambda
longChains' :: Int
longChains' = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))

--  with `map`, `filter` and `where`
longChains'' :: Int
longChains'' = length (filter isLong (map collatzChain [1..100]))
    where isLong xs = length xs > 15

-- mapping functions with multiple params. We can map functions that take multiple parameters
    -- by partially applying them and then applying them again if we need to. For example
    -- In the following example, by providing (*) to `map`, we're partially applying it to all
    -- the elements of the list and getting an list of functions as a result.

{-
```
ghci> let listOfFuns = map (*) [0..]
ghci> (listOfFuns !! 4) 5
20
```
-}

-- `flip` with a lambda
flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

-- The most common use case with flip is calling it with just the function parameter,
    -- or the function parameter and one extra parameter, and then passing the resulting
    -- function on to a map or a zipWith:

{-
```
ghci> zipWith (flip (++)) ["love you", "love me"] ["i ", "you "]
["i love you","you love me"]
ghci> map (flip subtract 20) [1,2,3,4]
[19,18,17,16]
```
-}

-- implementation of `foldl`
{-
This is how `foldl` expands:
call: foldl f acc [x1..]

foldl f (f acc x1) [x2..]
foldl f (f (f acc x1) x2) [x3..]
foldl f (f (f (f acc x1) x2) x3) [x4..]
... and so on
-}
foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft _ acc [] = acc
foldLeft f acc (x:xs) = foldl f (f acc x) xs

-- implementation of `foldr`.
{-
This is how foldr expands:
call: foldr f acc [x1..]
f x1 (foldr f acc [x2..])
f x1 (f x2 (foldr f acc [x3..]))
f x1 (f x2 (f x3 (foldr f acc [x4..])))
... and so on
-}
foldRight :: (b -> a -> a) -> a -> [b] -> a
foldRight _ acc [] = acc
foldRight f acc (x:xs) = f x (foldRight f acc xs)

-- implementation of `foldl1` in terms of foldl
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "foldl1 called with empty list"
foldl1' f (x:xs) = foldl f x xs

-- implementation of `foldr1`. Instead of returning the acc, the base case (a single element list)
    -- returns the last element.
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "foldr1 called with empty list"
foldr1' _ [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

-- implementation of `sum` with foldl
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

-- Or to be more idiomatic and "curryingy":
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- implementing `map` with right fold
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- Again, taking advantage of currying. A pattern that emerges from these examples is that if
    -- the last parameter is being used as the last argument of the returning function call
    -- inside the method, you can omit it from the signature as well as the call because
    -- the result of your partial application is a function that expects that last argument.
mapCurry :: (a -> b) -> [a] -> [b]
mapCurry f = foldr (\x acc -> f x : acc) []

{-
It's interesting to note that while we could've used a left fold in the implementation of `map`,
the right fold is preferable since with a left fold we would have to use ++ (with cons and
left fold we'd get a reversed list) and `++` always has linear time complexity:

```
map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> acc ++ [f x]) []
```
-}

-- reverse with left fold and `:`
reverseWFold :: [a] -> [a]
reverseWFold = foldl (\ acc x -> x : acc) []

-- Amazing use of `flip`, courtesy hlint. Wow! is haskell awesome! I know I shouldn't be this
    -- surprised because this follows very logically from what we've seen so far
    -- but I'm still amazed!
reverseWFold' :: [a] -> [a]
reverseWFold' = foldl (flip (:)) []

-- Implementing filter with `fold`
filterWFold :: (a -> Bool) -> [a] -> [a]
filterWFold p = foldr (\ x acc -> if p x then x : acc else acc) []

-- Implementing `elem` with fold. Again using currying to avoid the last argument.
elemRF :: (Eq a) => a -> [a] -> Bool
elemRF y = foldRight (\x acc -> (x == y) || acc) False

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- Implementation of `and`. Returns True if all elements are `True`; otherwise `False`.
-- This is will work with infinite list of `False`, but not with infinite list of `True`
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

{-
The important thing to note is that if the folding function is a function that doesn't
need both its arguments to return a value like ours here (since it uses ||), right fold
will work on infinit lists. To take advantage of this, right fold should always be implemented
such that the folding function takes list element as the first argument; this is obviously
how haskell's `foldr` is implemented. If you implement `foldr` the way I've done in `foldrWrong`,
you can't do the following:
```
ghci> foldrWrong (&&) True (repeat False)
```
But the same expression will work with the normal `foldr` because `&& False` will immediately
evaluate to `False`. The recursive call isn't even made because `&& False` is enough for
evaluation. This is a good example of Haskell's laziness. In the `foldWrong` on the other hand,
the first argument to `&&` is a recursive call, which, with infinite lists is a recipe
for infinite recursion. We could make it work though:
```
ghci> foldrWrong (flip (&&)) True (repeat False)
```
This works but is unncessarily roundabout. Haskell designers knew what they were doing.
-}

foldrWrong :: (a -> b -> a) -> a -> [b] -> a
foldrWrong _ acc [] = acc
foldrWrong f acc (x:xs) = f (foldrWrong f acc xs) x

-- The scanl and scanr functions are like foldl and foldr, except they report
    --all the intermediate accumulator states in the form of a list.

{-
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
ghci> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]
ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]
-}

-- How many elements does it take for the sum of the square roots of all natural numbers
    -- to exceed 1,000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

{-
Use of the `$` function to convert something like:
ghci> sum (filter (> 10) (map (*2) [2..10]))

to something like:
ghci> sum $ filter (> 10) $ map (*2) [2..10]
80

Mapping function application over a list of functions
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]
[7.0,30.0,9.0,1.7320508075688772]
-}

-- definition of the function composition function, (.)
dot' :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
f `dot'` g = \ x -> f $ g x

-- mapping a list to all negative numbers
negatives :: [Integer]
negatives = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- Other examples
{-
ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
[-14,-15,-27]

ghci> replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
can be converted to:

> replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5] 
-}