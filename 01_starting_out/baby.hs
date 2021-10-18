doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNum :: (Ord p, Num p) => p -> p
doubleSmallNum x = if x < 100 then doubleMe x else x

boomBang :: Integral a => [a] -> [[Char]]
boomBang xs = [if odd x then "BOOM!" else "BANG!" | x <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

rightTriangles = [(a, b, c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]

{-
- Arithmetic Functions
    +, -, /, *
    - a negative number has to be surrounded by parens: 5 * (-3)
- Boolean Operators:
    - Values: True, False
    - &&, ||, not
    - =, /=
- Functions
    - 'Operators' are simply functions that are use in infix style
    - Non-symbol functions with two params can also be used in infix style by surrounding it with ``, eg: `div`.
    - `div` does integral division
    - `min 10, 11` yeilds 10
    - `max 10, 11` yeids 11
    - Function application has the highest precedence of all the operations in Haskell:
        - `succ 9 + max 5 4 + 1` evaluates to 16
        - and is equivalent to `(succ 9) + (max 5 4) + 1`
    - This means that if we want to get the successor of 9 * 10, we couldn’t simply write
        - `succ 9 * 10`, instead we'd write
        - succ (9 * 10)
    - `if condition then something else something else` is how the conditional works in haskell
    
    - A function name cannot begin with capital letter

- Lists
    - Lists are homogenous data structures
        - They store elements of the same type
        `lostNumbers = [4,8,15,16,23,42]`
    - List concatenation is done using the `++` operator
    - Strings are simply lists of characters
    - `++` is an O(n) operation, n being the length of the first list
    - Adding to the beginning of a list, a constant-time op, is done using the `:` operator, also called cons
    - A list like `[1, 2, 3]` can also be represented as `1:2:3:[]`. A recursive data-structure.
    - Index access is done using the `!!` operator: `[1, 2, 3] !! 0` yeilds 1. Exceeding list bounds results
        - in an exception
    - Lists can be compared in lexicographical order by using the normal comparison operators.
        - A non-empty list is always considered greater than an empty list.

    - The `head` function takes a list and returns its first element
    - The `last` function takes a list and returns its last element.
    - The `tail` function takes a list and returns the whole list except the first element.
    - The `init` function takes a list and returns the whole list except the last element.
    - These functions, when used on empty lists, result in errors.
    - The length function returns the length of a list
    - The `null` function checks if a length is empty
    - The `reverse` function reverses a list
    - The `take` function takes a list and a number n and returns n numbers of elements starting from the first
        - Taking more elements than the list has results in all the elements being returned
    - `drop` is similar to take except it drops n number of elements from the beginning
    - The `maximum` function takes a list of `Ord` elements and returns the maximum
    - The `minimum` function should be self-explanatory
    - The `sum` function sums up a list of numbers
    - The `product` multiplies ...
    - The `elem` function takes an element and a list and checks if the element is part of the list
        - 5 `elem` [1, 2, 3, 4, 5] yeilds True
    
-- Ranges
    - A simple range is just a list expressed succinctly as `[1..20]`
    - If you want a different step, use it like so: `[1,3..20]` yeilds [1, 3, 5, 7, ..., 19]
    - Can be used with Enum types
    - To make a range in reverse order do this, `[20,19..1]`.
    - Since Haskell is lazy, you can make infinite lists: `take 24 [13,26..]
    - `repeat` takes an element and reproduces it infinitely: `take 5 (repeat 5)` yeilds [5, 5, 5, 5, 5]
    - `cycle` takes a list and replicates its elements indefinitely to form an infinite list
        - `take 6 (cycle [1,2,3])` yeilds [1, 2, 3, 1, 2, 3]
    - `replicate` takes an element and the number of times to replicate it: `replicate 3 10` yeilds [10, 10, 10]

- List Comprehensions
    - Basically a haskell equivalent of `map` and `filter` combine into one.
     (Perhaps a simplistic take. Will revise after more info)

    - `[x * 2 | <- [1..10]]` can be read as "multiply each x by 2 where x belongs to the range [1..10]"
        - The return value should be obvious: [2, 4, 6, 8, 10, ..., 20]

    - A condition called "predicate" can be added to a comprehension to filter out elements:
        - `[x * 2 | <- [1..10], x >= 12]` yeilds [12, 14, 16, 18, 20]

    - `boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]`
    - We can include as many predicates as we want, all separated by commas.
        - For instance, if we wanted all numbers from 10 to 20 that are not 13, 15 or 19, we’d do:
        - `[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]` yeilds [10,11,12,14,16,17,18,20]

    - When drawing values from several lists, every combination of elements from these lists
        - is reflected in the resulting list:
        - `[x+y | x <- [1,2,3], y <- [10,100,1000]]`
        - `[11,101,1001,12,102,1002,13,103,1003]`

    - Using list comprehension to write a version of the `length` function:
        - `length' xs = sum [1 | _ <- xs]`

    - Comprehensions can be used on strings, too:
        - `removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]`

- Tuples
    - Used to store several heterogenous elements as a single value
    - Tuples have fixed size. Need to know size ahead of time
    - Examples (1, 3), (3, 'a', "Hello"), (50.1, 12, "abc")
    - One can think of them as simply structs without names
    - A list can be made only of tuples having the same type
    - The rigid typing of tuples means that you can't write, for example, a function to add an element
      to the end of a tuple. You'd have to do that for every kind of tuple you use in your program.

    - Pairs
        - `fst` takes a pair and returns the first element
        - `snd` takes a pair and returns the second element
        - The `zip` function produces a list of pairs:
            - `zip [1, 2, 3] ['a', 'b', 'c']` yeilds [(1, 'a'), (2, 'b'), (3, 'c')]
            - if one list is longer than the other the remaining part of the longer list is ignored
    - Last Example with comprehensions and tuples:
        - `let rightTriangles = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a],
          a^2 + b^2 == c^2]`
-}
