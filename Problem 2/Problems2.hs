module Problems2 where

import Data.Char (ord, digitToInt)
import Control.Arrow (Arrow(second))

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 2
===============================

The problems in this problem set are focused on recursion: iteration over
numeric and list types.  As before, you should consider the type signature as
part of the problem specification, so you should not change it.

A technical note: you are likely to write at least one function that runs
forever.  If you click "Evaluate" or "Refresh" and nothing happens at all, don't
just keep clicking the link.  Instead, at the bottom of the VS Code window, look
for the tiny text that says "Evaluating...".  Click that, and then in the pop-up
window click "Cancel"

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 1. Semi-factorial.
--
-- The semi-factorial of n, written n!!, consists of the product of every
-- *other* number from n down to 1.  For example:
--
--    5!! = 5 * 3 * 1 = 15
--    4!! = 4 * 2 = 8
--
-- Contrast the factorial, which is the product of *every* number from n down to
-- 1:
--
--    5! = 5 * 4 * 3 * 2 * 1 = 120
--
-- The factorial has an easy recursive definition:
--
--    0! = 1
--    n! = n * (n - 1)!
--
-- Your task is to develop a recursive definition of the semi-factorial.

semifact :: Int -> Int
semifact x
  | x > 0 = x * semifact (x-2)
  | x <= 0 = 1
  | otherwise = -x

-- >>> semifact 4
-- 8

-- >>> semifact
-- No instance for (Show (Int -> Int))
--   arising from a use of ‘evalPrint’
--   (maybe you haven't applied a function to enough arguments?)


--------------------------------------------------------------------------------
-- 2. The Collatz 
--
-- The Collatz sequence is given as follows:
--
--   1. If the current number n is 1, the sequence ends
--   2. If the current number n is even, the sequence continues with n/2
--   3. If the current number n is odd, the sequence continues with 3n+1
--
-- For example, the Collatz sequence starting from 4 is 4, 2, 1.  The Collatz
-- sequence starting from 7 is 7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1.
--
-- Your tasks is to write a function that, given a starting point, returns the
-- Collatz sequence from that starting point, as a list.  You need to know two
-- more things about lists to write this function:
--
--   * A constant list of elements x1, x2, x3 is written [x1, x2, x3].  In
--     particular, the constant list with no elements is written [].
--
--   * To prepend a *single* element onto a list, use the : operator.  For
--     example, `1 : [2,3,4]` is the same list as `[1,2,3,4]`.

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x
  | even x = x : collatz (div x 2)
  | otherwise = x : collatz (3 * x + 1)


-- >>> collatz 4
-- [4,2,1]

-- >>> collatz 7
-- [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

{-------------------------------------------------------------------------------

The next few problems all deal with lists.  You need to know a couple of
functions for manipulating lists:

  * the `null` function returns `True` for an empty list, and `False` for a
    non-empty list

  * The `head` function returns the first element of a non-empty list, and
    crashes if applied to an empty list.

  * The `tail` function returns everything *but* the first element of a
    non-empty list, and crashes if applied to an empty list.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 3. Write a function which returns the odd-indexed elements of a list---that
--    is to say, the first, third, fifth, and so forth.  For example,
--    `oddIndexes ['a','b','c','d']` should return ['a','c']

oddIndexes :: [a] -> [a]
oddIndexes [] = []
oddIndexes [x] = [x]
oddIndexes (x:y:xs) = x : oddIndexes xs

-- Function used for next problem
evenIndexes :: [a] -> [a]
evenIndexes [] = []
evenIndexes [x] = []
evenIndexes (x:y:xs) = y : evenIndexes xs
-- >>> oddIndexes "abcde"
-- "ace"

-- >>> evenIndexes "abcde"
-- "bd"

-- >>> oddIndexes [0..20]
-- [0,2,4,6,8,10,12,14,16,18,20]

-- >>> oddIndexes [1..20]
-- [1,3,5,7,9,11,13,15,17,19]

--------------------------------------------------------------------------------
-- 4. Write a function which, given a list [x1, x2, x3, x4, ...], computes x1 -
--    x2 + x3 - x4 + ....  Given an empty list, you should return 0.

alternating :: [Int] -> Int
alternating x = sum (oddIndexes x) - sum (evenIndexes x)

-- >>> alternating [1,2,3,4,9]
-- 7

{-------------------------------------------------------------------------------

The next two functions deal with checksums.  The basic idea is to build some
validation into the format of identification numbers, so that simple errors
(typing 1 instead of 2, or switching digits) will produce invalid numbers
instead of just different numbers.  Many numeric formats use checksums,
including credit card numbers, ISBN book numbers, and so forth.

A couple more functions you may find it helpful to know for these problems:

 - The `ord` function turns a character into it's ASCII equivalent, so:

>>> ord 'a'
97

 - The `length` function returns the length of a list.  `take n xs` returns the
   first `n` elements of `xs`, while `drop n xs` returns the list without its
   first `n` elements.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

5. We'll start with a simple checksum strategy.  You should write a function
   `checkValidSimple` which returns True iff the last two digits of its argument
   string are equal to the (mod 100) sum of the preceding digits of the strings.
   For example, the string

     1204412317

   is valid according to this definition, because 1+2+0+4+4+1+2+3 = 17.  On the
   other hand, the string

     2992014129

  is invalid, because 2+9+9+2+0+1+4+1 = 28 ≠ 29.

(Remember that a Haskell `String` is just a `[Char]`)

-------------------------------------------------------------------------------}

checkValidSimple :: String -> Bool
checkValidSimple x
    | (sum (map charToInt (take (length x - 2) x))) `mod` 100 == grabLast (reverse(take 2 (reverse x))) = True
    | otherwise = False


grabLast :: [Char] -> Int
grabLast x = (ord (head x) - 48) * 10 + (ord (x !! 1) - 48)

charToInt :: Char -> Int
charToInt x = ord x - 48

-- >>> convertStoI ['2','8','0']
-- 14

checker :: String -> Int
checker x = grabLast (reverse(take 2 (reverse x)))
-- >>> checkValidSimple [1,2,3,4,0,0]
-- No instance for (Num Char) arising from the literal ‘1’

-- >>> checkValidSimple "1204412317"
-- True

-- >>> checkValidSimple "123501"
-- False

-- >>> checkValidSimple "1234502"
-- False

{-------------------------------------------------------------------------------

6. The previous checksum algorithm doesn't actually do very well, unfortunately.
   While it detects single character errors (no digit is equal to itself mod
   100), it actually can't detect transpositions at all: a + b = b + a mod 100.

   Your final task is to write a function `checkValid` that implements a more
   robust checksum strategy, originally developed by Hans Peter Luhn at IBM.
   This is the scheme used in many credit card numbers, among other places. The
   algorithm is as follows:

    1) Starting from the *right* edge of the string, double every other digit.
       If this results in a 2-digit number, add those digits!

    2) Compute the sum of the original odd-numbered and transformed
       even-numbered digits.

    3) If the sum is equal to 0 mod 10, the input is valid.

   I think an example is essential here.  Suppose our input string is:

        4   9   9   2   7   3   9   8   7   1   6

   We start out by doubling every other number (starting from the right, so 6 is
   *not* doubled)

        4  18   9   4   7   6   9  16   7   2   6

   If we ended up with a two digit number, we add the digits:

        4   9   9   4   7   6   9   7   7   2   6

   Now we compute the sum of these numbers, mod 10

>>> (4+9+9+4+7+6+9+7+7+2+6) `mod` 10
0

   The result is 0, so the string is accepted.

-------------------------------------------------------------------------------}

checkValid :: String -> Bool
checkValid x = (grabFirstVal x + grabSecondVal x) `mod` 10 == 0

grabSecondVal :: [Char] -> Int
grabSecondVal x = sum (map secondValProcess (map mult (map charToInt (evenIndexes(reverse x)))))

grabFirstVal :: [Char] -> Int
grabFirstVal x = sum (map charToInt (oddIndexes(reverse x)))

mult :: Int -> Int
mult x = x * 2

secondValProcess :: Int -> Int
secondValProcess x = sum (map digitToInt $ show x)

-- >>> checkValid "6011000990139424"
-- True

-- >>> secondValProcess 504
-- [5,0,4]

-- >>> grabFirstVal "6011000990139424"
-- 21

-- >>> checkValid "49927398717"
-- False

-- >>> checkValid "1234567812345678"
-- False

-- >>> checkValid "1234567812345670"
-- True
