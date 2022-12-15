{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Problems10 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 10
================================

The past series of problems has explored the use of Haskell's type systems to
capture invariants: we've captured well-formedness of several kinds of
derivations, and well-typedness of programs in a simply-typed lambda calculus.

In this problem set, we further develop our simply-typed lambda calculus to add
two fundamental data constructors: pairs, also called products, which combine
two types into a single type, and Eithers, also called sums, which capture an
option between two types.

We're going to build an abstract syntax tree for this language that corresponds
to typing derivations---that is to say, having a term in our abstract syntax
tree will come with a guarantee that the term is well-typed.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Variables
---------

As in the previous problem set, we need a way to capture variables and their
typing.  We can reuse the development in that problem set wholesale; feel free
to copy your implementation of the vars function from PS9.

-------------------------------------------------------------------------------}

data Var :: [*] -> * -> * where
    VZ :: Var (t : ts) t
    VS :: Var ts u -> Var (t : ts) u

instance Show (Var ts t) where
    show z = show (toNat z) where
        toNat :: Var ts t -> Int
        toNat VZ = 0
        toNat (VS z) = 1 + toNat z

data Env :: [*] -> * where
    ENil  :: Env '[]
    ECons :: t -> Env ts -> Env (t : ts)

---------------------------------------------------------------------------------
-- Problem 0: Implement the var function, as in PS9.
var :: Var ts t -> Env ts -> t
var VZ (ECons a as) = a
var (VS s) (ECons a as) = var s as

{-------------------------------------------------------------------------------

Expressions
-----------

There are two new ideas in this problem set:

* Pairs, or products, which combine two types to make a single types.
* Eithers, or sums, which capture a choice between two types.

As should not be surprising at this point, each of these ideas will come with
expression forms to *introduce* (or construct) the type and expression forms to
*eliminate* (or use) the type.  These will *very* closely parallel the treatment
of conjunction and disjunction in predicate logic, just as the treatment of
functions *very* closely parallels the treatment of implication.

Here is the type of expressions.  The additions are discussed in more detail below.

-------------------------------------------------------------------------------}

data Expr :: [*] -> * -> *  where
    -- STLC
    Var   :: Var ts t -> Expr ts t
             -- A variable of type `t` in environment `ts` can be an expression of
             -- type `t` in environment `ts`
    Lam   :: Expr (t : ts) u -> Expr ts (t -> u)
             -- If the expression `m` has type `u` in environment `t : ts`, then
             -- abstracting over that term would account for the first variable
             -- (of type `t`), and so results in a term of type `t -> u` in
             -- environment `ts`
    App   :: Expr ts (t -> u) -> Expr ts t -> Expr ts u
             -- If expression `m` has type `t -> u` and `n` has type `t`, then the
             -- application of `m` to `n` has type `u`.

    -- Pairs
    Pair :: Expr ts t -> Expr ts u -> Expr ts (t, u)
             -- If expression `m` has type `t`, and expression `n` has type `u`,
             -- then the pair of `m` and `n` has type `(t, u)`
    PrjL :: Expr ts (t, u) -> Expr ts t
             -- If expression `m` has type `(t, u)`, then the left projection of
             -- `m` has type `t`
    PrjR :: Expr ts (t, u) -> Expr ts u
             -- If expression `m` has type `(t, u)`, then the right projection
             -- of `m` has type `u`

    -- Eithers
    InjL :: Expr ts t -> Expr ts (Either t u)
             -- If expression `m` has type `t`, then injecting `m` on the left
             -- gives a value of type `Either t u`, for any `u` that you want.
    InjR :: Expr ts u -> Expr ts (Either t u)
             -- If expression `m` has type `u`, then injecting `m` on the right
             -- gives a value of type `Either t u`, for any `t` that you want.
    Case :: Expr ts (Either t u) -> Expr (t : ts) v -> Expr (u : ts) v -> Expr ts v
             -- If: expression `l` has type `Either t u`; expression `m`, given
             -- a new variable of type `t`, has type `v`; and expression `n`,
             -- given a new variable of type `u`, has type `v`, then `Case l m
             -- n` has type `v`.

    -- Constants
    Const :: t -> Expr ts t
             -- Finally, to add a little more interest, we allow Haskell
             -- expressions of type `t` to be embedded in our language.

{-------------------------------------------------------------------------------

Pairs
-----

In Haskell, pairs are introduced with the syntax `(,)`, and eliminated using
pattern matching.  For example, a function that swaps the components of a pair
might be written:

swap :: (t, u) -> (u, t)
swap (x, y) = (y, x)

To introduce pairs, we can do things almost as they're done in Haskell; we write
`Pair m n` instead of `(m, n)`, but the meaning is the same.  For eliminating
pairs, we don't want to try to capture the full complexity of pattern matching.
Instead, we'll rely on expressions that project (that is, extract) the two
components of a pair.  The components of a pair are frequently identified as
"left" and "right"---in the example above, `x` is the left component of the
input pair and `y` is the right component---so the projections are called `PrjL`
(to project the left component) and `PrjR` to project the right component.

Notice that the treatment of pairs---the forms of the introduction and
elimination---is identical to the treatment of conjunction in natural deduction.

-------------------------------------------------------------------------------}

---------------------------------------------------------------------------------
-- Here's a function that creates a pair of copies of its argument.  The
-- corresponding Haskell term would be \x -> (x, x)
twoE :: Expr '[] (t -> (t, t))
twoE = Lam (Pair (Var VZ) (Var VZ))

--------------------------------------------------------------------------------
-- Here's a function that projects the left component of the left component of
-- its argument; the corresponding Haskell term would be \((x, y), z) -> x
twiceLeftE :: Expr '[] (((t, u), v) -> t)
twiceLeftE = Lam (PrjL (PrjL (Var VZ)))

--------------------------------------------------------------------------------
-- Problem 1: Write a function which swaps the components of a pair.
swapE :: Expr '[] ((t, u) -> (u, t))
swapE = Lam (Pair (PrjR (Var VZ)) (PrjL (Var VZ)))
--------------------------------------------------------------------------------
-- Problem 2: Write a function that reassociates a nested pair.
assocRightE :: Expr '[] (((t, u), v) -> (t, (u, v)))
-- (t, (u, v))

assocRightE = Lam (Pair (PrjL (PrjL (Var VZ))) (Pair(PrjR(PrjL(Var VZ))) (PrjR (Var VZ))))

--Lam   :: Expr (t : ts) u -> Expr ts (t -> u)
--Pair :: Expr ts t -> Expr ts u -> Expr ts (t, u)
--PrjL :: Expr ts (t, u) -> Expr ts t
--PrjR :: Expr ts (t, u) -> Expr ts u

{-------------------------------------------------------------------------------

Eithers
-------

Unlike its treatment of products, Haskell doesn't have built-in syntax for sums.
Instead, Haskell provides the data type `Either t u`; a value of type `Either t
u` represents *either* a `t` or a `u`; as for pairs, the first is called the
`Left` branch and the second is called the `Right` branch.  For example:

fromRight :: t -> Either u t -> t
fromRight x (Left y)  = x
fromRight x (Right y) = y

Our language certainly isn't going to have user-defined data types or arbitrary
equations.  However, we can still capture the essence of the `Either` type.

We provide two expression that introduce values of the `Either` type.  These are
traditionally called "injections" (they're the reverse of the "projections" for
the pair type), so our expressions are named `InjL` and `InjR`.

Eliminating the `Either` type is a little more complicated.  We have two things
going on: first, we have to figure out if we're on the left or right branch; and
second, we need access to the value stored in the branch.  In terms of the
example above: the two equations correspond to figuring out the branch, and the
variable `y` gets the value stored in the branch.

Rather than have equations, we're just going to support a `case` expression
form.  So, for the above definition we have:

fromRight :: t -> Either u t -> t
fromRight x y = case y of
    Left z  -> x
    Right z -> z

To capture the value stored in the branch, we can take advantage of our
representation of variables: we don't have to worry about names or the like, we
just add a new variable to the environment, getting the stored value.

Notice that the treatment of Eithers---the forms of the introduction and
elimination---is identical to the treatment of disjunction in natural deduction.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Here's the fromRight function above in our language.  Pay attention to the
-- numbering of variables in the branches: in the left branch, the 0th variable
-- is the contents of the Either (`z` above), the 1st variable is the inner
-- lambda-bound variable (`y` above), and the 2nd variable is the outer
-- lambda-bound variable.
fromRightE :: Expr '[] (t -> Either u t -> t)
fromRightE = Lam (Lam (Case (Var VZ)
                            (Var (VS (VS VZ)))
                            (Var VZ)))

--------------------------------------------------------------------------------
-- Here's an example that both deconstructs and reconstructs an Either type; you
-- wrote a similar Haskell function in PS3.
mapRightE :: Expr '[] ((u -> v) -> Either t u -> Either t v)
mapRightE = Lam (Lam (Case (Var VZ)
                           (InjL (Var VZ))
                           (InjR (App (Var (VS (VS VZ)))
                                      (Var VZ)))))

--------------------------------------------------------------------------------
-- Problem 3: Write a function that swaps the sides of an Either.
rotateE :: Expr '[] (Either t u -> Either u t)
rotateE = Lam (Case (Var VZ) (InjR (Var VZ)) (InjL (Var VZ)))
{-

    Lam (Pair (PrjR (Var VZ)) (PrjL (Var VZ)))
    InjL :: Expr ts t -> Expr ts (Either t u)
             -- If expression `m` has type `t`, then injecting `m` on the left
             -- gives a value of type `Either t u`, for any `u` that you want.
    InjR :: Expr ts u -> Expr ts (Either t u)
             -- If expression `m` has type `u`, then injecting `m` on the right
             -- gives a value of type `Either t u`, for any `t` that you want.


             InJL or InJR             VZ value            VZ Value
    Case :: Expr ts (Either t u) -> Expr (t : ts) v -> Expr (u : ts) v -> Expr ts v

-}

{-------------------------------------------------------------------------------

Evaluation
----------

Finally, as in PS9, we can write an evaluation function for our `Expr` type.

We still have to interpret the features of simply-typed lambda calculus; feel
free to copy your implementations from PS9 (or from the PS9 sample solution).

We interpret pairs as Haskell pairs, and Eithers as Haskell Either's.  That
means:

 * The evaluation of `Pair m n` is a *Haskell* pair of the evaluation of `m` and
   `n`.
 * The evaluation of `InjL m` (resp. `InjR m`) uses the *Haskell* `Left` (resp.
   `Right`) constructor.

That means that we can implement the eliminations just as we would use the
Haskell types.

 * The evaluation of `PrjL m` (resp. `PrjR n`) should pattern match on the
   result of evaluating `m` (resp. `n`).
 * The evaluation of `Case l m n` should use either a helper function or a
   Haskell `case` to branch on the evaluation of `l`.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Problems 4-9:
eval :: Expr ts t -> Env ts -> t
eval (Var v) x   = var v x 
eval (App x y) z = eval x z (eval y z)
eval (Lam x) y   = \z -> eval x (ECons z y)
eval (Const x) y = x
eval (PrjL x) e = t
  where
  (t,u) = eval x e
eval (PrjR x) e = u
  where
  (t,u) = eval x e
eval (InjR x) e = Right (eval x e)
eval (InjL x) e = Left (eval x e)
eval (Pair m n) e =  (eval m e, eval n e)
eval (Case l m n) e = case eval l e of
  Left t -> eval m (ECons t e)
  Right u  -> eval n (ECons u e)