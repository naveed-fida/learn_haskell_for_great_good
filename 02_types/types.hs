factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

-- will return a float if float given, an int if int given, etc.
identity :: Num a => a -> a
identity x = 1

-- The :t command is used in GHCi to get the type of an expression
-- The :: character can be read as 'having type of'

-- Int is a bounded integer related to the machine's word size
-- Integer is an unnbounded integeral type that is slower to compute with
-- Float and Double are the usual floating point types
-- Bool is the Boolean type
-- Char is the character type. It is 16 bits (unicode)

-- Tuples can have an infinite number of types. It depends on the length as well as the types
    -- of the individual components

-- This is how to denote the type of a function :
    -- `funcName :: (TClass tvar, TClass tvar, ...) => tvar , -> tvar, ... -> tvar
    -- for an "any" type you can just use a variable without class constraints

-- The `Eq` type class provides an interface for checking equality between two values
-- The `Ord` type class is used for types that have an ordering among its different instances
-- The `compare` function takes an value belonging to the `Ord` type class and returns a value of `Ordering`.
    -- `Ordering` has three values: "LT", "EQ", "GT"
-- Values whos types are instances of the `Show` type can be represented as strings
-- The function `show` can be used to convert a `Show`-capable value to a string
-- The `Read` type class is the opposite of `Show`. The `read` function can be used to convert a string to
    -- a value whose type is an instance of `Read`
-- You have to use the result of `read` in a context that makes its desired type apparent or explicitly
    -- the type as in `read "5" :: Int
-- `String` and `[Char]` are the same type
-- `Enum` instances are sequentially ordered types. Their values can be enumerated. For example, they can
    -- be used with the `succ` and `pred` functions.
-- Instances of the `Bounded` type class have upper and lower bounds. Example: `maxBound :: Char` returns
    -- `'\1114111'`
-- Tuples whose components are all instances of `Bounded` are also instances of `Bounded`.
-- `Num` is a numeric type class. Its instances can act like numbers. Whole numbers are polymorphic constants
    -- meaning that they can act like any of `Float`, `Int`, etc.
-- `Floating` type class has `Float` and `Double` as its instances
-- `Integral` has `Int` and `Integer` as its instances.

-- A useful function to convert `Integral` numbers to `Num` is to `fromIntegral`.
-- Sometimes a type must first be an instance of one type class to be al- lowed to become an instance of
    -- another. For example, to be an instance of Ord, a type must first be an instance of Eq.

