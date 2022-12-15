module Problems1 where

import Prelude hiding (curry, even, uncurry)

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 1
===============================

This problem covers the basics of Haskell syntax.  Each problem asks you to
write a function given its description, type signature, and several test cases.
You should change the structure of the equations as you want: adding new
equations, parameters, guards, &c.  You should consider the type signature part
of the problem specification, so you should not change it.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part 1: Basic function definition
---------------------------------

In these problems, you should feel free to use any of the arithmetic operators
(+, -, *), logical operators (not, &&, ||), or comparison operators (==, /=, <,
&c.)    Haskell distinguishes between integer and fractional division: integer
division is written using the operators div and mod, whereas fractional division
is written with /.  

Remember that you can write either 

    div 4 2

or 

    4 `div` 2

The `` make a "normal" symbol into an operator.

-------------------------------------------------------------------------------}


-- 1.  Write a function that doubles its argument

double :: Int -> Int
double x = x + x

-- >>> double 2
-- 4

-- >>> double (-2)
-- -4

-- 2. Write a two-argument function that computes the product of its doubled
--    arguments.  Recall that multiple arguments are written by "just" adding
--    more arguments: "f x y" is function "f" applied to arguments "x" and "y"

doubleProd :: Int -> Int -> Int
doubleProd x y = (x + x) * (y + y)

-- >>> doubleProd 4 3
-- 48

-- >>> doubleProd 2 (-1)
-- -8

-- 3. Write a function that evaluates to true if its argument is even, and false
--    otherwise.  Recall that equations can be *guarded*: the following function
--    evaluates to M if P is true, and N otherwise:
--       f x | P         = M
--           | otherwise = N

even :: Int -> Bool
even x
      | mod x 2 == 0 = True
      | otherwise = False

-- >>> even 2
-- True

-- >>> even 3
-- False

-- >>> even 0
-- True

-- 4. Write a two argument function that evalutes to 1 if its second argument is
--    greater than or equal to its first argument, and to 0 otherwise.
step :: Int -> Int -> Int
step x y
  | y >= x = 1
  | otherwise = 0

-- >>> step 4 5
-- 1

-- >>> step 4 3
-- 0

{-------------------------------------------------------------------------------

Part 2: Pairs
-------------

Another of Haskell's built in types is the type of *pairs*.  Pairs---both types
and expressions---are written with (,).  For example, the type "(Int, Bool)"
denotes pairs of integers and Booleans; "(4, True)" is a value of this type.

When writing functions that use pairs, you have two choices.  There are
functions "fst" and "snd" that return the first and second components of a pair;
for example:

>>> fst (True, 1)
True

>>> snd (True, 1)
1

However, this is not a very "Haskell-y" way to do it.  Alternative, you can also
use (,) on the left-hand sides of your equations to take pair arguments apart.
This is an instance of a general idea called *pattern matching*.  For example,
we could define our own versions of the "fst" and "snd" functions by:

    myFst (x, y) = x
    mySnd (x, y) = y

In the following questions, you can use either "fst" and "snd" or pattern
matching.  However, I *strongly* recommend that you start practicing with
pattern matching now; it's an essential feature of Haskell, and the sooner
you're comfortable with it the better your semester is likely to go.    

-------------------------------------------------------------------------------}

-- 5. Write a function which computes the sum of a pair of integers.
pairSum :: (Int, Int) -> Int
pairSum (x,y) = x + y

-- >>> pairSum (1, 4)
-- 5

-- >>> pairSum (2, -4)
-- -2

-- 6. Write a two-argument function which, given a pair of integers (a, b) as
--    its first argument and a single integers x as its second, computes ax + b.
prodSum :: (Int, Int) -> Int -> Int
prodSum (x, y) z = (x*z) + y

-- >>> prodSum (2, 4) 3
-- 10

-- >>> prodSum (2, 5) (-3)
-- -1

-- 7. Write a function on integer pairs which:
--      (a) is *not* the identity function: for example, applying your function
--      to (1,2) should produce a result other than (1,2)
--      (b) is its own inverse---for example, the result of f (f (1, 2)) should
--      be (1, 2)
selfInv :: (Int, Int) -> (Int, Int)
selfInv (x, y) = (-1*x, -1*y)

-- >>> selfInv (1,2)
-- (-1,-2)

-- >>> selfInv (-1,-2)
-- (1,2)

-- 8. Write a self-inverse non-identity function that operates on pairs of
--    *arbitrary* typed values---that is to say, it works equally well on pairs
--    (Int,Int), pairs (Bool,Bool), and so forth.  In Haskell, we write
--    arbitrary types using *type variables*---the "a" in the type below can be
--    filled in by any type we want.
--
--    It's okay if this is the same function that you used for 7.  If it is,
--    tho, you might find it interesting to think about a solution to number 7
--    that is not a solution to number 8.
selfInv' :: (a, a) -> (a, a)
selfInv' (y, x) = (x, y)

-- >>> selfInv' (-1,-2)
-- (-2,-1)


-- Generic types, like the type of selfInv' above, can actually be quite strong
-- specifications.  There are, of course, many many functions of type "(Int,
-- Int) -> (Int, Int)".  But how many functions of type "(a, a) -> (a, a)" can
-- you think of?

{-------------------------------------------------------------------------------

Part 3: Higher-order functions
------------------------------

A "higher-order" function is a function that takes other functions as arguments,
or produces other functions as results.  Of course, because Haskell is a
functional programming language, all functions in Haskell can be higher order;
there's no special syntax or different naming convention for higher-order
functions.

For example, the compose function behaves like the composition operator from
high school algebra:

    compose :: (b -> c) -> (a -> b) -> a -> c
    compose f g x = f (g x)

Recall that in Haskell, the composition operator is spelled ".": so "f . g" is
"compose f g".

-------------------------------------------------------------------------------}

-- 9. The order of arguments to composition is not always intuitive---if we
--    think about the order in which we do things, "f . g" means "do f after
--    doing g".  Write a function that combines functions in the opposite order.
pipe :: (a -> b) -> (b -> c) -> a -> c
pipe a b c = b (a c)

-- >>> pipe double (+1) 4
-- 9

-- >>> pipe (+1) double 4
-- 10

-- 10. Write a two argument function which, given a function "f" and a pair "p", returns
--     the same pair but with "f" applied to the first component of "p"
applyFst :: (a -> c) -> (a, b) -> (c, b)
applyFst a (b, c) = (a b, c)

-- >>> applyFst double (1, 2)
-- (2,2)

-- >>> applyFst not (True, 2)
-- (False,2)

-- It may occur to you that we now have two ways we could handle passing
-- multiple arguments to functions---either the way we have been doing it "f x
-- y", or by using pairs of arguments "f (x, y)".  The next two problems show
-- that these are equivalent, by giving functions that transform between the two
-- views.

-- 11. Write a function which, given a function that expects a pair as its first
--     argument, instead behaves like an equivalent function of two arguments. 
curry :: ((a, b) -> c) -> a -> b -> c
curry a b c = a(b,c)

-- >>> curry (\(x, y) -> x + y) 2 3
-- 5

-- >>> curry (\(b, c) -> b && c) True False
-- False

-- 12. Write a function which, given a function that expects two arguments,
--     instead behaves like an equivalent function that expects a pair as its
--     argument.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry a (x, y) = a x y

-- >>> uncurry (+) (2,3)
-- 5

-- >>> uncurry (&&) (True, False)
-- False

{-------------------------------------------------------------------------------

Part 4: Lists
-------------

The final built-in type we're going to consider is lists.  Lists in Haskell are
written with []: [Int] is the type of lists of ints, and [1,2,3] is a value of
that type.

Normally, to write functions over lists, we'd need at least one more language
feature than we've seen so far: something like iteration, or looping, or
recursion.  But with higher-order functions in Haskell, we can actually describe
a fair number of list computations without needing to resort to those kinds of
features.  Here are some of Haskell's list-manipulating functions:

  - map :: (a -> b) -> [a] -> [b]

    "map f xs" computes the result of applying function "f" to each element in
    "xs".  For example:

>>> map not [True, False]
[False,True]

  - filter :: (a -> Bool) -> [a] -> [a]

    "filter f xs" returns a list consisting of those elements of "xs" for which
    "f" computes True.  For example:

>>> filter (0 ==) [0,1,2,3]
[0]

  - sum :: [Int] -> Int
  - length :: [a] -> Int

    These functions are more self-explanatory: "sum xs" computes the sum of the
    values in "xs", whereas "length xs" computes the number of elements in "xs".

-------------------------------------------------------------------------------}

-- 13. Write a function which, given a list of integers, returns a list in which
--     each of the integers has been doubled.

doubled :: [Int] -> [Int]
doubled = map double


-- >>> doubled [2,1,3]
-- [4,2,6]

-- 14. Write a function which, given a list of integers "xs", returns a list of
--     the even values in "xs".
evens :: [Int] -> [Int]
evens = filter even


-- >>> evens [0..5]
-- [0,2,4]

-- 15. Write a function which, given a list of integers "xs", returns the
--     *number* of positive integers in "xs".
countPositive :: [Int] -> Int
countPositive xy = length [ x | x <- xy, x > 0]

-- >>> countPositive [-2..2]
-- 2

-- 16. Write a function which computes the average of a list of integers.
--     Remember that Haskell distinguishes between integer and floating point
--     division---you can't just apply the floating point division operator / to
--     two integers.  To convert integers to doubles (or indeed to any of many
--     other types) use the function "fromIntegral".
average :: [Int] -> Double
average xy = fromIntegral (sum xy) / fromIntegral (length xy)

-- >>> average [1..4]
-- 2.5

-- >>> average [0..4]
-- 2.0
