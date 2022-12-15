module Problems5 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 5
===============================

This problem set continues the development of the Boolean formula language from
last problem set.  This week, our goal is to consider transformations of Boolean
formulae.  That is, we're interested in ways we can convert a given formula to
equivalent (but preferable) formulae.  You can think of this as being a simple
kind of optimization problem.

-------------------------------------------------------------------------------}

-- Here's my version of the formula type from last week---It's perfectly fine if
-- this isn't quite the same as the one you developed.

data Formula = V String              -- Variables
             | C Bool                -- Constants (truth and falsity)
             | Not Formula           -- Negation
             | Formula :/\: Formula  -- Conjunction
             | Formula :\/: Formula  -- Disjunction
  deriving (Eq, Show)

-- Specify grouping and precedence for conjunction and disjunction constructors

infixl 3 :/\:
infixl 2 :\/:

{-------------------------------------------------------------------------------

Part 1: Negation normal form
-----------------------------

Your first task is to translate a formula into "negation normal form" (NNF).  A
formula is in NNF if negation is *only* applied to propositional variables.  So,
the following formulae are in NNF:

    ¬A ∧ B
    A ∨ (¬A ∧ B)
    ⊤

The following formulae are *not* in NNF:

    ¬(A ∧ B)
    ¬⊥

To transform a formula into NNF, you will need to make use of the following
equations:

    ¬⊤ = ⊥
    ¬⊥ = ⊤
    ¬¬ɸ = ɸ
    ¬(ɸ ∧ ψ) = ¬ɸ ∨ ¬ψ
    ¬(ɸ ∨ ψ) = ¬ɸ ∧ ¬ψ

Of course, you need to apply those equations *wherever applicable* throughout
the formula—that is, you'll need to recursively traverse the formula looking for
instances of the equations.

This task will be assessed in five parts:

1. Negation of constants
2. Double negation
3. Negation over ∧
4. Negation over ∨
5. Nested negations

-------------------------------------------------------------------------------}

nnf :: Formula -> Formula
nnf (V x) = V x
nnf (C x) = C x
nnf (Not (Not x)) = nnf x
nnf (Not (C x)) = nnf (C (not x))
nnf (Not (x :\/: y)) = nnf(Not x) :/\: nnf (Not y)
nnf (Not (x :/\: y)) = nnf(Not x) :\/: nnf (Not y)
nnf (x :\/: y) = nnf x :\/: nnf y
nnf (x :/\: y) = nnf x :/\: nnf y
nnf x = x




-- >>> nnf (Not (Not (C False)))
-- C False

-- >>> nnf (Not (C False))
-- C True

-- >>> nnf (Not (V "A" :/\: Not (V "B")))
-- Not (V "A") :\/: V "B"

-- In case it helps, here is a function that *tests* whether or not a given
-- formula is in NNF.  Note that you should not ever need to call "isNnf" from
-- your "nnf" function!

isNnf :: Formula -> Bool
isNnf (V _) = True
isNnf (C _) = True
isNnf (Not (V _)) = True
isNnf (Not _) = False
isNnf (f :/\: g) = isNnf f && isNnf g
isNnf (f :\/: g) = isNnf f && isNnf g

-- >>> isNnf (Not (Not (C False)))
-- False

-- >>> isNnf (Not (V "A" :/\: Not (V "B")))
-- False

-- >>> isNnf (Not (V "A") :\/: V "B")
-- True

{-------------------------------------------------------------------------------

Part 2: Disjunctive normal form
-------------------------------

Your second task is to translate a formula *in NNF* into "disjunctive normal
form" (DNF).  A formula is in disjunctive normal form if it has no disjunctions
under conjunctions, or, equivalently, if it consists of a (possibly trivial)
disjunction of (possibly trivial) conjunctions.  For example, the following
formulae are in DNF:

    A ∨ (B ∧ C) ∨ (¬A ∧ C)
    A ∧ B
    ¬A

The following formula is *not* in DNF:

    A ∧ (B ∨ C)

To convert formulae to DNF, you will need the following equations:

    x ∧ (ψ ∨ η) = (x ∧ ψ) ∨ (x ∧ η)
    (ϕ ∨ ψ) ∧ η = (ϕ ∧ η) ∨ (ψ ∧ η)

Again: you *should assume* your input formula is *already* in NNF.

This task will be assessed in three parts:

6. Distributing conjunction from the left
7. Distributing conjunction from the right
8. Nested disjunction

-------------------------------------------------------------------------------}

isDisjunc :: Formula -> Bool
isDisjunc (x:\/:y) = True
isDisjunc x = False



dnf :: Formula -> Formula
dnf (V x) = V x
dnf (C x) = C x
dnf (Not (C x)) = dnf (C (not x))
dnf (Not (V x)) = Not (V x)
dnf (Not (Not x)) = dnf x
dnf (Not (x:\/:y)) = dnf (Not x) :/\: dnf (Not y)
dnf (Not (x:/\:y)) = dnf (Not x) :\/: dnf (Not y)
dnf (x :/\: (y :\/: z)) = dnf (x :/\: y) :\/: dnf (x :/\: z)
dnf ((x:\/: y) :/\: z) = dnf (x :/\: z) :\/: dnf (y :/\: z)
dnf (x:\/:y) = dnf x :\/: dnf y
dnf (x:/\:y)
    | isDisjunc (dnf x) || isDisjunc (dnf y) = dnf(dnf x :/\: dnf y)
    | otherwise = dnf x :/\: dnf y

-- >>> dnf (V "A" :/\: (V "B" :\/: V "C"))
-- V "A" :/\: V "B" :\/: (V "A" :\/: V "C")

-- >>> dnf (V "A" :/\: (V "B" :/\: (V "C" :\/: V "D")))
-- V "A" :/\: (V "B" :/\: (V "C" :\/: V "D"))

-- In case it helps, here is a function that *tests* whether or not a given
-- formula in NNF is also in DNF.  Again, you should not ever need to call
-- "isDnf" from your "dnf" function.

isDnf :: Formula -> Bool
isDnf = disj
    where disj (f :\/: g) = disj f && disj g
          disj (f :/\: g) = conj f && conj g
          disj f          = True
          conj (f :\/: g) = False
          conj (f :/\: g) = conj f && conj g
          conj f          = True

-- >>> isDnf (V "A" :/\: (V "B" :/\: (V "C" :\/: V "D")))
-- False

-- >>> isDnf (V "A" :/\: (V "B" :/\: V "C") :\/: V "A" :/\: (V "B" :/\: V "D"))
-- True
