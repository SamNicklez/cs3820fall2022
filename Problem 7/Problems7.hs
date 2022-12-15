{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Problems7 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 7
===============================

This problem explores judgments and derivations, and how we can realize them as
datatypes in Haskell.  (There is a deeper connection between Haskell programs
and logical derivations, which we will discuss at the end of the semester.)

Part 1: Formalizing the ≤ relation on ℕ
----------------------------------------

Our starting point is going to be derivation systems for the "less than or equal
to" relation on natural numbers.  We'll start by defining the natural numbers
(ℕ): ℕ consists of 0, along with the successor `S n` of every element `n ∈ ℕ`.
Or, to write it in Haskell:

-------------------------------------------------------------------------------}

data Nat = Z | S Nat

-- Let's introduce some shortcuts to avoid having to stack `S` constructors.

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four
type Six   = S Five
type Seven = S Six

{-------------------------------------------------------------------------------

There's more than one way we could write a derivation system for the ≤ relation
on ℕ.  Here's one attempt:

    (Zlen) ----------
           0 ≤ n

           m ≤ n
    (SleS) ---------
           S m ≤ S n

That is to say: we have two rules.  The first rule captures that 0 is less than
or equal to all other natural numbers.  The second captures that the successor
of m is less than the successor of n if m is less than n.  For example, if we
wanted to prove that 1 ≤ 4 (where I write 1 for the successor of 0, and 4 for
the successor of the successor of the successor of 1), we could give the
following derivation:

    (Zlen) ------
           0 ≤ 3
    (SleS) ------
           1 ≤ 4

The next question is how we can represent these derivations in Haskell.  We
could try to define a normal data type to capture such derivations, like:

    data Lte = Zlen | SleS Lte

The problem is that this data type captures nothing about how derivations relate
to the judgments they prove: the value `SleS Zlen`, for example, is not
inherently any more a proof of `1 ≤ 4` or `1 ≤ 7` than it is `0 ≤ 2` or even `2
≤ 0`.

To connect these pieces, we can use a feature of modern Haskell called
"generalized algebraic data types".  The idea is that we can define a type `Lte
m n` that *only* contains values that prove that `m ≤ n`.  Our data type looks
something like this:

-------------------------------------------------------------------------------}

data Lte1 :: Nat -> Nat -> * where
    Zlen :: Lte1 Z n
    SleS :: Lte1 m n -> Lte1 (S m) (S n)

{-------------------------------------------------------------------------------

The `Zlen` constructor can *only* have types `Lte Z n` for any natural number
`n`.  So, for example, the following generates a type error:

>>> Zlen :: Lte One One
Couldn't match type 'Z with 'S Zero
Expected: Lte One One
  Actual: Lte 'Z One
In the expression: Zlen :: Lte One One
In an equation for `it_akK8': it_akK8 = Zlen :: Lte One One

This doesn't mean that 1 isn't ≤ 1; it just means that the `Zero` rule doesn't
*prove* that `1 ≤ 1`.  (You can ignore all the single quotes in the error
message; those are essentially an implementation detail of the compiler.) Here's
another example:

>>> SleS Zlen :: Lte (S (S Z)) (S (S (S (S Z))))
Couldn't match type 'Z with 'S 'Z
Expected: Lte ('S 'Z) ('S ('S ('S 'Z)))
  Actual: Lte 'Z ('S ('S ('S 'Z)))
In the first argument of `SleS', namely `Zlen'
In the expression: SleS Zlen :: Lte (S (S Z)) (S (S (S (S Z))))
In an equation for `it_akM2':
    it_akM2 = SleS Zlen :: Lte (S (S Z)) (S (S (S (S Z))))

On the other hand, here's an example that works:

-------------------------------------------------------------------------------}

pr0_1 :: Lte1 Two Four
pr0_1 = SleS (SleS Zlen)

{-------------------------------------------------------------------------------

You can tell this example works because it type checks!

Well, almost.  There is one way to produce something that seems to type, but
doesn't really correspond to the kind of proof we'd like:

-------------------------------------------------------------------------------}

pr0_2 :: Lte1 One Zero
pr0_2 = pr0_2

{-------------------------------------------------------------------------------

This claims to be a proof that 1 ≤ 0, something we'd probably prefer not to be
able to prove.  Of course, there's no derivation in our derivation system that
would actually prove such a claim, but `pr0_2` gets around that by never
producing any result at all!  To rule this kind of thing out, we'll define a
function that only completes given a finite argument:

-------------------------------------------------------------------------------}

fin1 :: Lte1 m n -> ()
fin1 Zlen       = ()
fin1 (SleS lte) = fin1 lte

{-------------------------------------------------------------------------------

Now we can say that a value `p` of the right type, and such that `fin1 p`
evaluates, is a proof.  We can even capture this using a silly `Show` instance:

-------------------------------------------------------------------------------}

instance Show (Lte1 m n) where
    show lte = case fin1 lte of () -> "proved!"

{-------------------------------------------------------------------------------

Now, we can see that `pr0_1` is a proof:

>>> pr0_1
proved!

But if we did the same thing for pr0_2, we'd never get the string "proved!",
because `finite1 pr0_2` will never actually return.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Your first problems are to build terms (finite terms, that is) inhabiting the
following types; or, if you prefer, your first problems are to prove that:

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Problem 1: 0 ≤ 5
pr1_1 :: Lte1 Zero Five
pr1_1 = Zlen
--------------------------------------------------------------------------------
-- 3 ≤ 5
pr1_2 :: Lte1 Three Five
pr1_2 = SleS (SleS (SleS Zlen))

--------------------------------------------------------------------------------
-- 3 ≤ 7
pr1_3 :: Lte1 Three Seven
pr1_3 = SleS (SleS (SleS Zlen))

{-------------------------------------------------------------------------------

Part 2: Formalizing the reflexive transitive closure
----------------------------------------------------

Another way we can think of formalizing ≤ is as the reflexive, transitive
closure of a simpler relation, which I'll call <₁.   The simpler relation
captures exactly one step of a less than proof:

    (Lt1) --------
          n <₁ S n

That is to say: each number is 1 less than it's immediate successor.  The idea
of the reflexive transitive closure of a relation is that it captures
*sequences* of that relation, of length ≥ 0.  Let's (just for the moment) write
the reflexive transitive closure of <₁ as <₁*.  Then we would have:

    4 <₁* 4

(because that's 0 instances of <₁)

    4 <₁* 5

(because that's 1 instance of <₁)

    4 <₁* 8

(because that's 3 instances of <₁)

and so forth.  As you won't be surprised to notice, the reflexive transitive
closure of <₁ is exactly the ≤ relation.

-------------------------------------------------------------------------------}

data Lt1 :: Nat -> Nat -> * where
    Lt1 :: Lt1 m (S m)

fin2_1 :: Lt1 m n -> ()
fin2_1 Lt1 = ()

{-------------------------------------------------------------------------------

Now we just have to formalize the reflexive transitive closure of `Lt1`. One way
we could do this is to represent the reflexive and transitive cases directly; we
might end up with the following rules:


    (Refl)   ------
             n ≤ n

             m <₁ n
    (Inst)   -------
             m ≤ n

    (Trans)  m ≤ n       n ≤ p
             -----------------
             m ≤ p

The (Inst) rule might seem silly---why not just have `m ≤ S m`, but the point is
that this is a *general* construction, applicable to relations beyond just <₁.

We can capture this construction in Haskell as follows.

-------------------------------------------------------------------------------}

data Lte2 :: Nat -> Nat -> * where
    Refl  :: Lte2 m m
    Inst  :: Lt1 m n -> Lte2 m n
    Trans :: Lte2 m n -> Lte2 n p -> Lte2 m p

fin2 :: Lte2 m n -> ()
fin2 Refl               = ()
fin2 (Inst lt1)         = fin2_1 lt1
fin2 (Trans lte3 lte3') = fin2 lte3 `seq` fin2 lte3'

instance Show (Lte2 m n) where
    show lte = fin2 lte `seq` "proved!"

{-------------------------------------------------------------------------------

And again, we can prove that 2 ≤ 3, for example:

>>> Trans (Inst Lt1) Refl :: Lte2 Two Three
proved!

but not

>>> Trans Refl Refl :: Lte2 Two Three
Couldn't match type 'Z with 'S Zero
Expected: Lte2 Two Three
  Actual: Lte2 Two Two
In the second argument of `Trans', namely `Refl'
In the expression: Trans Refl Refl :: Lte2 Two Three
In an equation for `it_aH4K':
    it_aH4K = Trans Refl Refl :: Lte2 Two Three

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Your final problems are, once again, to prove that

* 0 ≤ 5
* 3 ≤ 5
* 3 ≤ 7

with our second formalization of ≤, by giving terms that inhabit the types
below. However, there is a twist.  This formalization can provide multiple
different proofs of the same judgment.  You should demonstrate this by providing
two *different* proofs that 0 ≤ 4 (as `pr3_1a` and `pr3_1b`)

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Problem 4: 0 ≤ 5
pr2_1a :: Lte2 Zero Five
pr2_1a = Trans (Trans (Trans (Trans (Trans (Inst Lt1) (Inst Lt1)) (Inst Lt1)) (Inst Lt1)) (Inst Lt1)) Refl

--------------------------------------------------------------------------------
-- Problem 5: A different proof that 0 ≤ 5
pr2_1b :: Lte2 Zero Five
pr2_1b = Trans (Trans (Trans (Trans (Trans (Trans (Inst Lt1) (Inst Lt1)) (Inst Lt1)) (Inst Lt1)) (Inst Lt1)) Refl) Refl

--------------------------------------------------------------------------------
-- Problem 6: 3 ≤ 5
pr2_2 :: Lte2 Three Five
pr2_2 = Trans (Inst Lt1) (Inst Lt1)

--------------------------------------------------------------------------------
-- Problem 7: 3 ≤ 7
pr2_3 :: Lte2 Three Seven
pr2_3 = Trans (Trans (Inst Lt1) (Inst Lt1)) (Trans (Inst Lt1) (Inst Lt1))




