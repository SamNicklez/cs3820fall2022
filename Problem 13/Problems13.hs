module Problems13 where

import Bird.Parser

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 13
=============================================

This problem applies parser combinators to several parsing problems.


-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Problem 1
---------

The first problem is to parse strings of equal numbers of 'a's and 'b's.  For
example, the strings "aaabbb" and "ab" should parse, while the string "aaaabb"
does not.  You should include the empty string as having an equal number of 'a's
and 'b's.  Note that your parser may not consume all the input!  For example,
given a string "aaabbbb", your parser should consume "aaabbb" and leave the
final 'b' on the input.

-------------------------------------------------------------------------------}

balanced :: Parser [Char]
balanced = option "" $ do char 'a'
                          cs <- balanced
                          char 'b'
                          return ('a' : cs ++ "b")

-- >>> runParser balanced "aaabbb"
-- [("aaabbb","")]

-- >>> runParser balanced "aaaaaaabbbbbbbbb"
-- [("aaaaaaabbbbbbb","bb")]

-- >>> runParser balanced "ccdd"
-- [("","ccdd")]

-- >>> runParser balanced "aaaaaabbbbb"
-- [("","aaaaaabbbbb")]

{-------------------------------------------------------------------------------

Problem 2
---------

The second problem is to write a parser combinator `manyn` that parses a fixed
number of repetitions of its input parser.  For example, the parser `manyn 3
(char 'a')` would succeed on input "aaa", but fail on input "aa".

-------------------------------------------------------------------------------}

manyn :: Int -> Parser a -> Parser [a]
manyn 0 p = return []
manyn n p = do x <- p
               xs <- manyn (n - 1) p
               return (x : xs)

{-------------------------------------------------------------------------------

Problem 3
---------

The third problem is to write a parser `balanced3` that parses strings of equal
numbers of 'a's, 'b's, and 'c's. For example, the strings "abc", "aaabbbccc",
and "" all parse, while the string "aaabbcc" and "aaabbbcc" do not.  The `manyn`
combinator from the previous problem should prove useful in writing the
`balanced3`.

-------------------------------------------------------------------------------}


balanced3 :: Parser [Char]
balanced3 = option "" $
            do as <- many1 (charr 'a')
               bs <- manyn (length as) (charr 'b')
               cs <- manyn (length as) (charr 'c')
               return (as ++ bs ++ cs)
    where
          charr c = do char c
                       return c

-- >>> runParser balanced3 "aaabbbccc"
-- [("aaabbbccc","")]

-- >>> runParser balanced3 "aaaabbbccc"
-- [("","aaaabbbccc")]

-- >>> runParser balanced3 "aaabbbcccc"
-- [("aaabbbccc","c")]

-- >>> runParser balanced3 "dddeeefff"
-- [("","dddeeefff")]


{-------------------------------------------------------------------------------

Problem 4
---------

The final task is to define a parser for floating point numbers, following this
pattern:

    (-)?{digit}*(.{digit}+)?

That is, the following are all valid examples of float point numbers:

    4
    4.1
    -4
    -4.1
    .2
    -.34
    -0
    -0.0

The following are not

    4.
    --4
    -
    -.

Your parser should return the value parsed, as a Haskell `Double`

-------------------------------------------------------------------------------}


float :: Parser Double
float = do negative <- option id (char '-' >> return negate)
           whole    <- option Nothing (Just `fmap` nat)
           case whole of
               Nothing -> do char '.'
                             ds <- many1 digit
                             return (negative (makeFraction ds / 10))
               Just w -> do ds <- option [] (char '.' >> many1 digit)
                            return (negative (fromIntegral w + makeFraction ds / 10))
    where makeFraction :: [Int] -> Double
          makeFraction []       = 0
          makeFraction (d : ds) = fromIntegral d + makeFraction ds / 10

-- >>> runParser float "25"
-- [(25.0,"")]

-- >>> runParser float "2.5"
-- [(2.5,"")]

-- >>> runParser float ".5"
-- [(0.5,"")]

-- >>> runParser float "-2.5"
-- [(-2.5,"")]

-- >>> runParser float "-.2551"
-- [(-0.2551,"")]

-- >>> runParser float "4."
-- [(4.0,".")]

-- >>> runParser float "-ab3"
-- []



