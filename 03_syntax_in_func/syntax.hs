-- pattern matching with function definition. Patterns fall through. The last pattern should be a
    -- catch-all to account for all patterns
lucky :: Int -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, you're out of luck, pal!"

-- another contrived example of pattern matching
sayMe1 :: Int -> String
sayMe1 1 = "One"
sayMe1 2 = "Two"
sayMe1 3 = "Three"
sayMe1 4 = "Four"
sayMe1 5 = "Five"
sayMe1 x = "Not Between One and Five!"

-- alternative `sayMe`: Not a book example
sayMe :: Int -> String
sayMe x =
    let dict = ["One", "Two", "Three", "Four", "Five"] in
        if x >= 1 && x <= 5 then
            dict !! (x - 1) else
            "Not between one and five!"

-- pattern matching (catch-all in this instance) with tuples as parameters
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- recursion using patterns
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

-- extracting first element from a triple
first :: (a, b, c) -> a
first (a, _, _) = a

-- extracting the second element from a tuple
second :: (a, b, c) -> b
second (_, b, c) = b

-- extracting the third element from a tuple
third :: (a, b, c) -> c
third (_, _, c) = c

-- You can also pattern-match inside list comprehensions. If a pattern-match fails, it will move
    -- to the next element and not include that element in the resulting list.
    {-
        ```
        ghci> let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
        ghci> [a+b | (a, b) <- xs]
        [4,7,6,8,11,4]
        ```
    -}

-- homegrown version of `head`. The `x:xs` pattern matches the head, `x` and the tail, `xs`
    -- a pattern that binds several variables has to be surrounded by ()
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- a more detailed example of list pattern matching. `[x]` and `[x,y]` can also be rewritten as
    -- `x:[]` and `x:y:[]` etc.
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell [x,y,z] = "This list has three elements. The first two elements are: " ++ show x
                  ++ " and " ++ show y
tell (x:y:z:_) = "This is way too long. The first three elements are " ++ show x ++ ", " ++
                 show y ++ " and " ++ show z

-- The "as" pattern allows to split up an item according to a pattern while still keeping a reference
    -- to the original item
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- A guard is denoted by the | character and are used to run some boolean checks on the values
    -- that pass through a pattern. The otherwise guard takes care of values that didn't pass
    -- any of the other guards
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight. Eat a bit, mate!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some fucking weight, you fat bastard!"
    | otherwise = "You're a whale. Congratulations!"

-- a homegrown version of max
max' :: (Ord a) => a -> a -> a
max' a b
    | a >= b = a
    |otherwise = b

-- functions can be DEFINED as well as used with backticks
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b    = EQ
    | a < b     = LT
    | otherwise = GT

-- `where` is used to defined variables for use in a function definition, a list comprehension or
    -- a pattern body. `where` bindings are not shared across function bodies of different patterns.
    -- note that we're using pattern matching to bind variables to values inside the `where` block
bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight. Eat a bit, mate!"
    | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some fucking weight, you fat bastard!"
    | otherwise = "You're a whale. Congratulations!"
    where bmi = weight / height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- A different implementation of `bmiTell` with `where`. The vertical alignment of the
    -- bindings is a syntactical requirement.
bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | skinny = "You're underweight. Eat a bit, mate!"
    | normal = "You're supposedly normal. Pfft, I bet you're ugly!"
    | fat = "You're fat! Lose some fucking weight, you fat bastard!"
    | otherwise = "You're a whale. Congratulations!"
    where bmi = weight / height^2
          skinny = bmi <= 18.5
          normal = bmi <= 25.0
          fat = bmi <= 30

-- list pattern-matching in `where` block
initials' :: String -> String -> String
initials' firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

-- more idiomatic alternative to the above implementation
initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- Function definition in `where` block and pattern matching in list comprehension
bmis :: [(Double, Double)] -> [Double]
bmis xs = [bmi w h | (w, h) <- xs]
    where bmi wt ht = wt / ht ^ 2

-- `let` expressions. An important difference between `let` and while is that `while` is a block
    -- and `let` is an expression and also very local so it can be used inside other expressions.
    -- Like `where` you can also define functions in a let expression
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- If you want to make several bindings, you can use a semicolon like so:
    -- (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
    -- (6000000,"Hey there!")

-- Pattern matching with let expressions can be very useful for quickly dis- mantling a
    -- tuple into components and binding those components to names, like this:
    {-
        ```
        ghci> (let (a, b, c) = (1, 2, 3) in a+b+c) * 100
        600
        ```
    -}

-- `let` in list comprehension
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- `let` and a predicate in list comprehension
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- pattern matching with `case`. The parameter pattern matching is simply syntactic sugar
    -- for `case`. As is obvious from the example, `case` makes an expression. The vertical
    -- lining up of the patterns is a syntactic requirement
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- head' redefined with `case`
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

-- Another way to write `describeList`
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."