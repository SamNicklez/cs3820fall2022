module Problems3 where

import Data.Char (ord, chr)

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 3

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

We begin with a couple of functions on pairs.  You can use pairs just like any
other constructor in Haskell---pattern matching to deconstruct arguments on the
left-hand sides of equations, and constructing results on the right-hand side.

I encourage you to write these functions using pattern matching, not using the
fst and snd functions (or their analogues.)

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 1. Write a function "split" that applies two functions to a common argument,
--    returning a pair of results.

split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g x = (f x, g x)

-- >>> split (1+) (0==) 0
-- (1,True)

-- >>> split (1+) (0==) 1
-- (2,False)

-- >>> split ord succ 'b'
-- (98,'c')



--------------------------------------------------------------------------------
-- 2. Write a function "times" that applies one function to each component of a
--    pair.

times :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
times f g (x, y) = (f x, g y)

--------------------------------------------------------------------------------
-- 3. Write a pair of functions "leftAssoc" and "rightAssoc" that illustrate
--    that the pair constructor is associative.  (To make this a better
--    argument, we would want to show that leftAssoc . rightAssoc = id, and vice
--    versa...).

leftAssoc :: (a, (b, c)) -> ((a, b), c)
leftAssoc (x, (y, z)) = ((x, y), z)

-- >>> leftAssoc (1,(True, "foo"))
-- ((1,True),"foo")

rightAssoc :: ((a, b), c) -> (a, (b, c))
rightAssoc ((x, y), z) = (x, (y, z))

-- >>> rightAssoc (("True", True), 1)
-- ("True",(True,1))

-- >>> leftAssoc (rightAssoc (("True", True), 1))
-- (("True",True),1)

{-------------------------------------------------------------------------------

"Maybe" is defined in the Haskell standard libraries, but there's nothing
special about its definition at all.  It's just:

data Maybe a = Nothing | Just a

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 4. Write a function "maybeDefault" which returns the value inside a Maybe
--    value, if it's there, or a default value otherwise.

maybeDefault :: a -> Maybe a -> a
maybeDefault _ (Just x) = x
maybeDefault x Nothing  = x

-- >>> maybeDefault 1 (Just 2)
-- 2

-- >>> maybeDefault 1 Nothing
-- 1

--------------------------------------------------------------------------------
-- 5. Write a function "maybeBranch" which applies a function to the value
--    stored inside a Maybe value, if it's there, or returns a default value
--    otherwise.

maybeBranch :: (a -> b) -> b -> Maybe a -> b
maybeBranch f _ (Just x) = f x
maybeBranch _ x Nothing  = x

-- >>> maybeDefault 1 (Just 2)
-- 2

-- >>> maybeDefault 1 Nothing
-- 1

--------------------------------------------------------------------------------
-- 6. Write a function "maybeMap", which applies a function to the value stored
--    inside a Maybe value (if it's there), building a new Maybe value as a
--    result.

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just (f x)
maybeMap _ Nothing = Nothing

-- >>> maybeMap ord (Just 'a')
-- Just 97

-- >>> maybeMap ord Nothing
-- Nothing

{-------------------------------------------------------------------------------

"Either" is similarly just a datatype definition in the standard libraries:

data Either a b = Left a | Right b

You should be able to see that "Maybe" is really a special case of "Either"...

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 7. Write a pair of functions "leftDefault" and "rightDefault" that extract
--    the Right (resp. Left) case of an Either value, returning a default for
--    the other case.

leftDefault :: b -> Either a b -> b
leftDefault x (Left _) = x
leftDefault _ (Right y) = y

rightDefault :: a -> Either a b -> a
rightDefault _ (Left x) = x
rightDefault x (Right _) = x

-- >>> leftDefault 'a' (Left 'c')
-- 'a'

-- >>> rightDefault 'a' (Left 'c')
-- 'c'

--------------------------------------------------------------------------------
-- 8. Write a function "eitherBranch" which, given one function for each case
--    and an Either value, applies the appropriate function to the contents of
--    Either.

eitherBranch :: (a -> c) -> (b -> c) -> Either a b -> c
eitherBranch f _ (Left x) = f x
eitherBranch _ g (Right y) = g y

-- >>> eitherBranch ord succ (Left 'a')
-- 97

-- >>> eitherBranch ord succ (Right 1)
-- 2

--------------------------------------------------------------------------------
-- 9. Write a function "plus" which, given two functions and an Either value,
--    applies the function that matches its contents and rebuilds an Either
--    value with the result.

plus :: (a -> b) -> (c -> d) -> Either a c -> Either b d
plus f _ (Left x) = Left (f x)
plus _ g (Right y) = Right (g y)

-- >>> plus ord succ (Left 'a')
-- Left 97

-- >>> plus ord succ (Right 'b')
-- Right 'c'

{-------------------------------------------------------------------------------

Lists, like pairs, are just data types with special syntax.  We could imagine
them having been introduced with a declaration:

data [a] = [] | a : [a]

If [] weren't reserved syntax.  Regardless, you can treat them like other data
types: a list function can be defined over multiple equations, for [] or (:)
cases, and can bind the head and tail of a list as you would bind the components
of a pair.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 10. Write a function "prefix s t" which returns True if s is a prefix of t
--     (that is, if the first length s characters of t are s).

prefix :: String -> String -> Bool
prefix [] s              = True
prefix (c : cs) (d : ds) = c == d && prefix cs ds
prefix _ _               = False

-- >>> prefix "foo" "foobar"
-- True

-- >>> prefix "Foo" "foobar"
-- False

-- >>> prefix "foo" "foo"
-- True

-- >>> prefix "foobar" "foo"
-- False

--------------------------------------------------------------------------------
-- 11. Write a function "commonPrefix s t" which returns the prefix that both
--     "s" and "t" have in common.  (Of course, this may be all of s or t, or
--     may be the empty string.)

commonPrefix :: String -> String -> String
commonPrefix (c : cs) (d : ds)
  | c == d = c : commonPrefix cs ds
commonPrefix _ _ = []

-- >>> commonPrefix "plant" "paper"
-- "p"

-- >>> commonPrefix "plant" "planter"
-- "plant"

-- >>> commonPrefix "paper" "plant"
-- "p"

-- >>> commonPrefix "paper" "rock"
-- ""

{-------------------------------------------------------------------------------

For the final part of this problem set, we're going to consider a data structure
called a Radix tree.  The goal of a Radix tree is to be a more efficient
(space-wise) search tree for data.  For example, suppose that we were storing
(case-insensitive) strings of letters.  A naive approach would introduce a
26-way branch at each node of the tree---quite a lot of branching to store, say,
the single word "umbrella".

A radix tree instead stores *strings*, representing the common prefix of all the
children of that node.  For example, the tree containing only "pants" would be:

    |
    + "pants"

The root only has one child, labeled "pants".  That node, in turn has no
children.

Now suppose we insert "paper".  "paper" and "pants" have "pa" as a common
prefix, so the resulting tree would look like:

    |
    + "pa"
       |
       + "nts"
       |
       + "per"

Now we insert "umbrella".  This has no common prefix with "pants" or "paper"
(that is: it has no common prefix with "pa"), so it gets a new node at the top
level.

    |
    + "pa"
    |  |
    |  + "nts"
    |  |
    |  + "per"
    |
    + "umbrella"

Finally, we insert "paperweight".  This has one of the existing nodes, "paper",
as a prefix.  Note the entry we add at "paper" to indicate that "paper" is an
entry in the tree, not just the prefix of "paperweight"

    |
    + "pa"
    |  |
    |  + "nts"
    |  |
    |  + "per"
    |     |
    |     + ""
    |     |
    |     + "weight"
    |
    + "umbrella"

For this problem, we're going to make two important simplifying assumptions.
First, we're not going to maintain the internal nodes in sorted order---for
example, we're going to treat the tree above equivalent to:

    |
    + "pa"
    |  |
    |  + "per"
    |  |  |
    |  |  + ""
    |  |  |
    |  |  + "weight"
    |  |
    |  + "nts"
    |
    + "umbrella"

Second, we're going to assume that every string we insert comes with an ending
symbol that is guaranteed not to appear in the middle of any string---for
example, instead of inserting "pants", "paper" and "paperweight", we'll assume
we're inserting "pants.", "paper.", and "paperweight."  This is a simplification
because it means that we don't have to account for the case in which a string in
the tree is a proper prefix of another string.  Concretely, instead of the trees
above, we'll have trees like:

    + "pa"
    |  |
    |  + "per"
    |  |  |
    |  |  + "."
    |  |  |
    |  |  + "weight."
    |  |
    |  + "nts."

And we won't have to specially take account of empty nodes.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

We start with the definition of the radix tree type.  Each node in the tree
(including the root!) contains a list of branches.  Each branch contains a
prefix string, and the tree from that prefix.

For a proper radix tree, we would maintain this list in sorted order.  We're not
going to worry about that for this problem set.

-------------------------------------------------------------------------------}

data Radix = Branch [(String, Radix)]
  deriving (Eq, Show)

-- The empty radix tree has no branches.

empty :: Radix
empty = Branch []

isEmpty :: Radix -> Bool
isEmpty (Branch bs) = null bs

-- Here are some example trees

t1, t2, t3 :: Radix

t1 = Branch [("p", Branch [("lant.", Branch []), ("aper.", Branch [])])]

t2 = Branch [("p", Branch [("lant.", Branch []), ("aper.", Branch [])]), ("umbrella.", Branch [])]

t3 = Branch [("t", Branch [("est", Branch [(".", Branch []), ("er.", Branch [])]), ("oast.", Branch [])])]

--------------------------------------------------------------------------------
-- 12. Write a function "strings" which returns a list of the strings (only the
--     full strings, not the prefixes by themselves!) contained in a radix tree.
--     You can return this list in *any* order; you do not have to reproduce the
--     order used in the sample outputs.

strings :: Radix -> [String]
strings (Branch []) = [""]
strings (Branch bs) = concatMap each bs
    where each (prefix, tree) = map (prefix ++) (strings tree)

-- >>> strings t1
-- ["plant","paper"]

-- >>> strings t2
-- ["plant","paper","umbrella"]

-- >>> strings t3
-- ["test","tester","toast"]

--------------------------------------------------------------------------------
-- 13. Write a function "insert" which inserts a new string into a radix tree.
--     If the string is already in the tree, you should return the original tree
--     unchanged.  Note: you do *not* have to maintain branches in sorted order,
--     or in the same order that appears in the sample outputs.
--
--     HINT: you will most likely want to write a function that walks over a
--     list of branches attempting to insert your new string.  For each branch,
--     you should consider three cases:
--
--        1. The new string and the branch have no common prefix.
--        2. The branch is identical to the new string
--        3. The branch is a prefix of the new string.
--        4. The new string and the branch have a common prefix.
--
--     In the first case, you will need to keep looking.  In the second case,
--     you are done; congratulations!  In the other cases, you will want to
--     build a new branch containing both the existing branch and the new
--     string.
--
--     HINT: You did not write the prefix and commonPrefix functions for
--     nothing.

insert :: String -> Radix -> Radix
insert [] r          = r
insert s (Branch bs) = Branch (walk bs)
    where walk [] = [(s, empty)]
          walk ((t, b) : bs)
            | prefix t s = (t, insert (drop (length t) s) b) : bs
            | not (null u) = (u, Branch [(drop (length u) t, b), (drop (length u) s, empty)]) : bs
            | otherwise = (t, b) : walk bs
            where u = commonPrefix s t


-- >>> insert "toast." empty
-- Branch [("toast",Branch [])]

-- >>> strings (insert "toast." empty)
-- ["toast"]

-- >>> insert "papier." t1
-- Branch [("p",Branch [("lant",Branch []),("ap",Branch [("er",Branch []),("ier",Branch [])])])]

-- >>> strings (insert "papier." t1)
-- ["plant","paper","papier"]

-- >>> insert "paperweight." t2
-- Branch [("p",Branch [("lant",Branch []),("aper",Branch [("",Branch []),("weight",Branch [])])]),("umbrella",Branch [])]

-- >>> strings (insert "paperweight." t2)
-- ["plant","paper","paperweight","umbrella"]

-- >>> insert "apple." t3
-- Branch [("t",Branch [("est",Branch [("",Branch []),("er",Branch [])]),("oast",Branch [])]),("apple",Branch [])]

-- >>> strings (insert "apple." t3)
-- ["test","tester","toast","apple"]

-- >>> insert "total." t3
-- Branch [("t",Branch [("est",Branch [("",Branch []),("er",Branch [])]),("o",Branch [("ast",Branch []),("tal",Branch [])])])]

-- >>> strings (insert "total." t3)
-- ["test.","tester.","toast.","total."]
