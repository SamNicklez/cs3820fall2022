{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, RankNTypes, QuantifiedConstraints #-}
module Problems8 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 8
===============================

This problem continues our exploration of derivations and their representation
in Haskell.  As in the last problem, we're going to be considering the reflexive
transitive closure of relations.   This week makes two advances over last week's
code: first, we're going to consider generic encodings of the reflexive
transitive closure; second, we're going to consider multiple encodings of the
reflexive transitive closure and discover the relationships between them.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- As last week, we start with a data type for natural numbers, and some
-- shortcuts to avoid having to stack `S` constructors.

data Nat = Z | S Nat

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four
type Six   = S Five
type Seven = S Six

{-------------------------------------------------------------------------------

Last week, we developed a "single-step" version of the less than relation:

    (Lt1) --------
          n <₁ S n

This week, we add a "single-step" version of the greater than relation:

    (Gt1) --------
          n >₁ S n

-------------------------------------------------------------------------------}

data Lt1 :: Nat -> Nat -> * where
    Lt1 :: Lt1 m (S m)

data Gt1 :: Nat -> Nat -> * where
    Gt1 :: Gt1 (S m) m

{-------------------------------------------------------------------------------

Part 1: Formalizing the reflexive-transitive closure
----------------------------------------------------

Last week, we developed a reflexive-transitive closure specific to the `Lt1`
relation: the `Inst` constructor required an argument of type `Lt1`.  In
general, however, that is not necessary.  Here is a generic version of the
reflexive transitive closure.  Notice that this is parameterized by a
single-step relation, such as `Lt1` or `Gt1`.

-------------------------------------------------------------------------------}

data Rtc :: (Nat -> Nat -> *) -> Nat -> Nat -> * where
    Refl  :: Rtc r m m
    Inst  :: r m n -> Rtc r m n
    Trans :: Rtc r m n -> Rtc r n p -> Rtc r m p

---------------------------------------------------------------------------------
-- Problem 1: Show that 3 ≤ 5

p1 :: Rtc Lt1 Three Five
p1 = Trans (Inst Lt1) (Inst Lt1)

---------------------------------------------------------------------------------
-- Problem 2: Show that 5 ≥ 3

p2 :: Rtc Gt1 Five Three
p2 = Trans (Inst Gt1) (Inst Gt1)

{-------------------------------------------------------------------------------

The `Rtc` type is not the only way we could formalize the reflexive transitive
closure.  We could also make the observation that the reflexive transitive
closure of a relation R consists of 0 or more steps of R.  For example:

    3 ≤ 3:   0 steps of <₁
    3 ≤ 4:   1 steps of <₁
    3 ≤ 5:   2 steps of <₁
    3 ≤ 6:   3 steps of <₁

And so forth.  We can capture this description of the reflexive transitive
closure in Haskell as well.

-------------------------------------------------------------------------------}

data Star :: (Nat -> Nat -> *) -> Nat -> Nat -> * where
    Done :: Star r m m
    Step :: r m n -> Star r n p -> Star r m p

{-------------------------------------------------------------------------------

For example, to show that 2 ≤ 3, we could use:

>>> Step Lt1 Done :: Star Lt1 Two Three
proved!

or, conversely, that 3 ≥ 2:

>>> Step Gt1 Done :: Star Gt1 Three Two
proved!

Of course, we need the right number of steps; it won't do to take too few:

>>> Done :: Star Gt1 Three Two
Couldn't match type 'S Zero with 'Z
Expected: Star Gt1 Three Two
  Actual: Star Gt1 Three Three
In the expression: Done :: Star Gt1 Three Two
In an equation for `it_a22h6':
    it_a22h6 = Done :: Star Gt1 Three Two

Or too many:

>>> Step Gt1 (Step Gt1 Done) :: Star Gt1 Three Two
Couldn't match type 'Z with 'S Zero
Expected: Star Gt1 One Two
  Actual: Star Gt1 One One
In the second argument of `Step', namely `Done'
In the second argument of `Step', namely `(Step Gt1 Done)'
In the expression: Step Gt1 (Step Gt1 Done) :: Star Gt1 Three Two

-------------------------------------------------------------------------------}

---------------------------------------------------------------------------------
-- Problem 3: Show that 3 ≤ 5

p3 :: Star Lt1 Three Five
p3 = Step Lt1 (Step Lt1 Done)

---------------------------------------------------------------------------------
-- Problem 4: Show that 5 ≥ 3

p4 :: Star Gt1 Five Three
p4 = Step Gt1 (Step Gt1 Done)

{-------------------------------------------------------------------------------

Part 2: Manipulating proofs
----------------------------

So far, we've only constructed proofs.  However, our proofs are just Haskell
data types: that means we can use them as inputs, not just results, analyze them
by pattern matching, and so on.

For a simple example, here's a function that determines whether a proof is by
reflexivity (or by a method equivalent to reflexivity):

------------------------------------------------------------------------------}

isRefl :: Rtc p m n -> Bool
isRefl Refl        = True
isRefl (Inst _)    = False
isRefl (Trans p q) = isRefl p && isRefl q

-- >>> isRefl (Trans (Inst Gt1) Refl)
-- False

-- >>> isRefl (Trans Refl (Trans Refl Refl))
-- True

{-------------------------------------------------------------------------------

Your next task is to show that ≥ is converse to ≤.  That is, to show that (for
arbitrary m and n), if

    m ≤ n

then

    n ≥ m

You'll do this by writing a function `convRtc` that maps from values in `Rtc Lt1
m n` (that is to say: proofs of m ≤ n) to values in `Rtc Gt1 n m` (that is to
say: proofs of n ≥ m).

This is *much* easier to do with the `Rtc` representation of the reflexive
transitive closure.  (If you don't believe me, try the other one yourself.)
We'll return to the `Star` representation at the end of of the problem.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------

-- Problem 5
convRtc :: Rtc Lt1 m n -> Rtc Gt1 n m
convRtc Refl = Refl
convRtc (Inst Lt1) = Inst Gt1
convRtc (Trans a b) = Trans (convRtc b) (convRtc a)
-- >>> convRtc (Trans (Inst Lt1) (Inst Lt1)) :: Rtc Gt1 Three One
-- proved!


{-------------------------------------------------------------------------------

The two representations of the reflexive transitive closure should be
equivalent.  I've asserted that they are, but in this problem you'll demonstrate
it.  To do so, you'll write two functions, `rtcToStar` and `starToRtc` that
convert between the representations.

In writing `rtcToStar`, you might find yourself at a bit of an impasse in the
`Trans` case.  If so, consider writing a helper function with the type

    Star r m n -> Star r n p -> Star r m p

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Problem 6
starToRtc :: Star r m p -> Rtc r m p
starToRtc Done = Refl
starToRtc (Step x Done) = Inst x
starToRtc (Step x y) = Trans (Inst x) (starToRtc y)

-- >>> starToRtc (Step Lt1 (Step Lt1 Done)) :: Rtc Lt1 Zero Two
-- proved!

--------------------------------------------------------------------------------
-- Problem 7
rtcToStar :: Rtc r m p -> Star r m p
rtcToStar Refl = Done
rtcToStar (Inst x) = Step x Done
rtcToStar (Trans x y) = helperfunc (rtcToStar x) (rtcToStar  y)

helperfunc :: Star r m n -> Star r n p -> Star r m p
helperfunc (Step x nOne) y = Step x (helperfunc nOne y)
helperfunc Done (Step nTwo x) = Step nTwo x
helperfunc Done Done = Done

-- >>> rtcToStar (Trans (Inst Gt1) (Inst Gt1)) :: Star Gt1 Two Zero
-- proved!


{-------------------------------------------------------------------------------

Now that we can map between the representations of reflexive transitive
closures, we can finish the converse argument we started in problem 5.  For your
final task, you'll write a function `conv1` that shows that ≥ is the converse
of ≤ with the Star representation of the reflexive transitive closure as well.

Note: you should *not* have to do any pattern matching in writing your solution.

-------------------------------------------------------------------------------}

---------------------------------------------------------------------------------
-- Problem 8
convStar :: Star Lt1 m n -> Star Gt1 n m
convStar =  rtcToStar . convRtc . starToRtc

-- >>> convStar (Step Lt1 (Step Lt1 Done)) :: Star Gt1 Three One
-- proved!

----------------------------------------------------------------------------------
-- Here be dragons!
--
-- To avoid the series of strangely named "finiteness" functions from PS7--you
-- can safely ignore all the `Proof` and `Show` instances.
----------------------------------------------------------------------------------

class Proof p where
    finite :: p -> ()

instance Proof (Lt1 m n) where
    finite Lt1 = ()

instance Show (Lt1 m n) where
    show p = finite p `seq` "proved!"

instance Show (Gt1 m n) where
    show p = finite p `seq` "proved!"

instance Proof (Gt1 m n) where
    finite Gt1 = ()

instance (forall m n. Proof (p m n)) => Proof (Rtc p m n) where
    finite Refl        = ()
    finite (Inst p)    = finite p
    finite (Trans p q) = finite p `seq` finite q

instance (forall m n. Proof (p m n)) => Show (Rtc p m n) where
    show p = finite p `seq` "proved!"

instance (forall m n. Proof (p m n)) => Proof (Star p m n) where
    finite Done       = ()
    finite (Step p r) = finite p `seq` finite r

instance (forall m n. Proof (p m n)) => Show (Star p m n) where
    show p = finite p `seq` "proved!"
