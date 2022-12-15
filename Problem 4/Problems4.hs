
module Problems4 where

import Prelude hiding (negate)

import Data.List (nub)
import Data.Bits (Bits(xor))

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 4
===============================

This problem set follows the development of a simple language---Boolean
formulae---embedded in Haskell.

Our language has five language features:

  - Constants: we can write the constant formulae truth "⊤" and falsity "⊥"
  - Variables: we have propositional variables, identified by arbitrary strings,
    representing unknown Boolean quantities. For example, "A", "B", "foo", and
    "bar" are propositional variables.
  - Negation: if ɸ is a formula of our language, then its negation ¬ɸ is also a
    formula of our language.
  - Conjunction and disjunction: if ɸ and ψ and formulae in our language, then
    their conjunction ɸ ∧ ψ and their disjunction ɸ ∨ ψ and also formulae of our
    language.

For example, "¬A ∧ (A ∨ (B ∧ A))" is a formula of our language.

Over the course of this problem set, you'll develop:

1. An abstract syntax for our language of formulae, and an interface for
   constructing formulae.

2. A simple semantics for formulae, parameterized by an interpretation of the
   propositional variables.

3. A semantic characterization of formulae, derived from your semantics.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part 1 (Problems 1-6)
---------------------

First, you need to define the abstract syntax tree for formulae and its
interface.

Your abstract syntax tree will be expressed with the data type "Formula".  Values
of this type will represent formulae, so you'll need to include constructors for
each possible kind of formula.  Of course, you should feel free to generalize
when possible: you can't include a distinct constructor for each possible
propositional variable (there are many strings...), so you'll want a single
constructor for all the variables.

I don't know how you'll choose to build your Formula type, so you'll also
implement an interface to your Formula type.  Your interface consists of six
functions:

 - The function "variable" builds propositional variables; for example,
   (variable "A") or (variable "foo")

 - The functions "truth" and "falsity" build the logical constants

 - The function "negate" builds the negation of a formula.  For example, (negate
   (variable "A")) builds the formula ¬A.

 - The functions "conj" and "disj" build conjunctions and disjunctions of
   formulae, respectively.  For ease of use, these functions operate on *lists*
   of formulae---instead of having to write (conj (variable "A") (conj (variable
   "B") (variable "C"))), we can write (conj [variable "A", variable "B",
   variable "C"]).  However, your functions should still behaves consistently
   with having binary conjunction and disjunction operations.  In particular:

     * (conj []) builds the same formula as (truth); (disj []) builds the same
       formula as (falsity).

     * (conj [f]) and (disj [f]) are the same as just f.

     * You should group conjunction and disjunction to the right.  That is:
       (conj [variable "A", variable "B", variable "C"]) builds the same formula
       as (conj [variable "A", conj [variable "B", variable "C"]).  This should
       *not* be the same formula as that built by (conj [conj [variable "A",
       variable "B"], variable "C"]).  The same holds for disj.

Each function in the interface counts as one problem.

-------------------------------------------------------------------------------}

data Formula =
      Var String
    | Negate Formula
    | Conj Formula Formula
    | Disj Formula Formula
    | T
    | F

  deriving (Eq, Show) -- This line asks Haskell to automatically figure out how to
                      -- compare values of your AST for equality and print them for
                      -- debugging tests.  You should leave it unchanged...


variable :: String -> Formula
truth, falsity :: Formula
negate :: Formula -> Formula
conj :: [Formula] -> Formula
disj :: [Formula] -> Formula


variable = Var
truth = T
falsity = F
negate = Negate
conj [x] = x
conj [] = truth
conj (x:xs) = Conj x (conj xs)
disj [] = falsity
disj [x] = x
disj (x:xs) = Disj x (disj xs)

-- >>> conj [variable "x", variable "y", variable "z"]
-- And (Var "x") (And (Var "y") (Var "z"))


{-------------------------------------------------------------------------------

Part 2 (Problems 7-12)
----------------------

The next task is to define a semantics for formulae, captured by a formula
called "eval" (for "evaluate").  The intuition here is the (hopefully) obvious
one: we interpret a formula by its Boolean value.  So, for example, the formulae
(truth) is interpreted as True, and (conj [truth, falsity]) is interpreted as
False.

This does not tell us how variables should be interpreted: does (variable "A")
mean True or False?  What about (conj [variable "A", variable "B"])?  To answer
these questions, our semantics will also be parameterized by a list of true
propositional variables, called the environment. Variables appearing in the
environment are interpreted as True, and variables not in the list are
interpreted as False.  So, if our environment is ["A"], we would interpret
(variable "A") as True, but (conj [variable "A", variable "B"]) as False (as "B"
does not appear in the environment, and so is interpreted as False).

While you'll only write one "eval" function, we'll count this as six problems:
one for each feature of the language.  Problem 7 is interpreting variables,
problem 8 is interpreting the truth constant, and so forth.

-------------------------------------------------------------------------------}

eval :: Formula -> [String] -> Bool
eval T _ = True
eval (Var a) xs = a `elem` xs
eval F _ = False
eval (Negate f) xs = not (eval f xs)
eval (Conj x y) xs = eval x xs && eval y xs
eval (Disj x y) xs = eval x xs || eval y xs

{-------------------------------------------------------------------------------

Part 3 (Problems 13-15)
-----------------------

Your final task is a characterization of formulae derived from your semantics. A
formula is unsatisfiable if it is False in every possible environment.  For
simple examples, the formula (conj [variable "A", negate (variable "A"])) is
unsatisfiable: in any environment that includes "A", negate (variable "A") is
interpreted as False; in any environment not including "A", (variable "A") is
interpreted as False.  On the other hand, the formula (conj [variable "A",
negate (variable "B")]) is not unsatisfiable: in any envionment including "A"
and not including "B", the formula will be interpreted as True.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- 13. To start, write a function "variables" that returns a list of all the
--     variables that appear in a formula.  Your list should be unique: even if
--     (variable "A") appears twice in the formula, you should only include it
--     once in your list.  However, order does not matter: for the formula (conj
--     [variable "A", variable "B"]), you could return either ["A", "B"] or
--     ["B", "A"].
--
--     You can ensure uniqueness only using the functions we've seen already,
--     but if you prefer you can use the function "nub", which removes all the
--     duplicate elements of a list.

variables :: Formula -> [String]
variables T = []
variables F = []
variables (Var x) = [x]
variables (Negate x) = variables x
variables (Conj x y) = nub(variables x ++variables y)
variables (Disj x y ) = nub(variables x ++variables y)

--------------------------------------------------------------------------------
-- 14. Write a function subsets such that "subsets xs" returns every subset of
--     xs.  For example, subsets ["A", "B"] should return [[], ["B"], ["A"],
--     ["A","B"]].  Order doesn't matter, but be sure to return both the
--     improper subset (that is, the list xs) and the trivial subset (that is,
--     the empty list).

subsets :: [String] -> [[String]]
subsets [] = [[]]
subsets (y:ys) = (subsets ys) ++ (map (y:) (subsets ys))

--------------------------------------------------------------------------------
-- 15. Write a function unsatisfiable such that "unsatisfiable f" returns True
--     if "f" is unsatisfiable.  You might find one of the functions:
--
--          and :: [Bool] -> Bool
--          all :: (a -> Bool) -> [a] -> Bool
--
--     helpful.  "and xs" returns True if all the "xs" are True; similarly, "all
--     p xs" returns True if p returns True for every element of xs.

unsatisfiable :: Formula -> Bool
unsatisfiable (Var x) = False
unsatisfiable (Conj x y) = unsatisfiable x || unsatisfiable y
unsatisfiable (Disj x y) = unsatisfiable x || unsatisfiable y
unsatisfiable (Negate x) = not (unsatisfiable x)
unsatisfiable F = True
unsatisfiable T = False
