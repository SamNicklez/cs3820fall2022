{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Problems9 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 9
===============================


In the past two problems, we've used Haskell's type system to represent
derivations as data types.  We've even guaranteed that those derivations are
well-formed.   In this problem, we'll extend the idea to terms in a simple typed
lambda calculus---that is to say, a programming language with variables,
functions, and function applications.  We're going to build an abstract syntax
tree for this language that corresponds to typing derivations---that is to say,
having a term in our abstract syntax tree will come with a guarantee that the
term is well-typed.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part 0: de Bruijn Notation
==========================

Of course, the real complexity is all in variables.  To represent variables,
we're going to use a technique called *de Bruijn notation*, named after the
Dutch computer scientist Nicolaas Govert de Bruijn.  The idea is that we'll
represent variables with numbers rather than names.  A variable's number
represents which enclosing lambda introduced it.  In turn, lambdas themselves
won't need to say anything about the variables they introduce at all.  That's
pretty terse; here are some examples.

The identity function would be written in Haskell as

    \x -> x

That is: the body is just a variable, which refers to the immediately enclosing
lambda.  With de Bruijn notation, we would represent this as:

    \ 0

That is to say: we have a lambda, enclosing a variable `0`.  The variable refers
to the immediately enclosing lambda---that is to say, the only one in the term.

The constant function would be written in Haskell as

    \x -> \y -> x

That is to say: A function from x to a function from y to x.  With de Bruijn
notation, we would represent this as

   \ \ 1

That is: we have two lambdas, as we did in Haskell, surround a variable 1.  The
variable doesn't refer to its immediately enclosing lambda (that would be `y` in
the Haskell example), but rather one further out (that's the `x`).

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part 1: Representing expressions
================================

So what's wrong with a data type like

    data Expr = Var Int | Lam Expr | App Expr Expr

Nothing, I suppose... we could write terms like the above as

    Lam (Var 0)
    Lam (Lam (Var 1))

but it includes a lot of expressions that don't actually make sense

    Lam (Var 1)

for example.  This is exactly where type systems are here to help.  The ideas
are two-fold:

* We should *only* be able to derive our typing judgment for expressions that
  have meaning; and,
* We should represent expressions by *their typing derivations*, not by the
  expressions alone.

Typing
------

So what will our type system look like?  We start, as always, with judgments.
Our judgments look like this:

Γ ⊢ M : T

where Γ is the typing environment (giving types to variables), M is the
expression we're typing, and T is its type.  M is given by the grammar above
(variables, functions, and applications), and we'll just use Haskell types for
T.  For Γ, we'll rely on the fact that we're using de Bruijn indexing: we can
just let Γ be a list, where the 0th variable has the type of the 0th list
element, the 1st variable has the type of the 1st list index, and so forth.

So what we want is not to have expressions to have expressions typed like

    Lam (Var 0) :: Expr

but rather to have expressions typed like:

    Lam (Var 0) :: Expr '[] (Int -> Int)

where the type captures that in an empty environment (that's the '[] part), the
expression has type Int -> Int.  To distinguish "lists of types" from "types of
lists", lists of types can't just be written [Int, Bool], but need to be written
'[Int, Bool].

Representing variables
----------------------

The real challenge here is going to be variables---how do we capture the
relationship between a variable, its typing environment, and its type?  Our
starting point is that we want to type variables just the way we type
expressions; the variable 0 should have a type like:

    0 :: Var '[Int, Bool] Int

That is: in the environment '[Int, Bool], variable 0 has type Int.  So then
what's the Bool part about?  Remember that this is part of the derivation of a
whole expression, so the environment will be the environment for the expression,
not just for one part of it.

There's a catch, of course: we can't use actual Haskell numbers that way.  But
we can do so using Peano numbers.  We want rules like this:

-------------------------------------------------------------------------------}

data Var :: [*] -> * -> * where
    VZ :: Var (t : ts) t
    VS :: Var ts u -> Var (t : ts) u


instance Show (Var ts t) where
    show z = show (toNat z) where
        toNat :: Var ts t -> Int
        toNat VZ = 0
        toNat (VS z) = 1 + toNat z


zero = Var VZ
one = Var . VS $ VZ
two = Var . VS . VS $ VZ
three = Var . VS . VS . VS $ VZ
four = Var . VS . VS . VS . VS $ VZ
five = Var . VS . VS . VS . VS . VS $ VZ


{-------------------------------------------------------------------------------

A few examples.

>>> VZ :: Var '[Int, Bool] Int
0

>>> VZ :: Var '[Int, Int, Int, Int, Int] Int
0

>>> VS (VS VZ) :: Var '[Int, Bool, Bool] Bool
2

As we expect, if we don't align our types correctly, we get a type error.  In
this case, the zero'th type in the list is not `Bool`:

>>> VZ :: Var '[Int, Bool] Bool
Couldn't match type ‘Int’ with ‘Bool’
Expected type: Var '[Int, Bool] Bool
  Actual type: Var '[Int, Bool] Int

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Representing expressions
------------------------

Now that we have variables sorted, we can define our expression type.  Again,
the idea is that an expression's type will identify both its typing environment
and its result type.  For example, the zeroth variable `Var VZ` would have type

    Expr '[Int, Bool] Int

the identity function would be represented by the term

    Lam (Var VZ)

That is: a lambda containing variable 0.  This term would have type

    Expr '[] (t -> t)

-------------------------------------------------------------------------------}

data Expr :: [*] -> * -> *  where
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
    Const :: t -> Expr ts t
             -- Finally, to add a little more interest, we allow Haskell
             -- expressions of type `t` to be embedded in our language.

parensIf n m s
    | n >  m    = "(" ++ s ++ ")"
    | otherwise = s

instance Show (Expr ts t) where
    showsPrec j (Var z)   s = show z ++ s
    showsPrec j (Lam m)   s = parensIf j 0 ("\\ " ++ showsPrec 0 m "") ++ s
    showsPrec j (App m n) s = parensIf j 1 (showsPrec 1 m  (' ' : showsPrec 2 n "")) ++ s
    showsPrec _ (Const _) s = "<>" ++ s

{-------------------------------------------------------------------------------

We can confirm our intuitions:

-------------------------------------------------------------------------------}

idE :: Expr '[] (t -> t)
idE = Lam (Var VZ)

constE :: Expr '[] (t -> u -> t)
constE = Lam (Lam (Var (VS VZ)))

ppaE :: Expr '[] (t -> (t -> u) -> u)
ppaE = Lam (Lam (App (Var VZ) (Var (VS VZ))))

-- >>> ppaE
-- \ \ 0 1

sE :: Expr '[] ((t -> u -> v) -> (t -> u) -> t -> v)
sE = Lam (Lam (Lam (App (App (Var (VS (VS VZ))) (Var VZ))
                        (App (Var (VS VZ)) (Var VZ)))))

-- >>> sE
-- \ \ \ 2 0 (1 0)

{-------------------------------------------------------------------------------

On the other hand, we can't give the identity function the type `Int -> Bool`:

>>> Lam (Var VZ) :: Expr '[] (Int -> Bool)
Couldn't match type ‘Int’ with ‘Bool’
Expected type: Expr '[] (Int -> Bool)
  Actual type: Expr '[] (Int -> Int)

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Your first task is to define several functions in our language:

 * the function `tsnocE` is the reverse of the `constE` function: it should be a
   two argument function that always returns its second argument.  This is
   captured in its type: given arguments of arbitrary types `t` and `u`, it
   returns a `u`.

 * The function `flipE` takes a function and then two arguments, and passes
   those arguments to the function in the opposite order.  Again, this is
   captured by its type.  First, it takes a `t -> u -> v` function.  Then it
   takes arguments of types `u` and `t` and returns a result `v`.

Of course, these functions could be perfectly well defined in Haskell.  To
receive credit, your definitions should not use the `Const` case of the `Expr`
type.

-------------------------------------------------------------------------------}

---------------------------------------------------------------------------------
-- Problem 1: tsnocE
tsnocE :: Expr '[] (t -> u -> u)
tsnocE = Lam $     -- λ x : t. (1)
         Lam $     -- λ y : u. (0)
         zero       -- y        

--------------------------------------------------------------------------------

-- Problem 2: flipE
flipE :: Expr '[] ((t -> u -> v) -> u -> t -> v)
flipE = Lam $  -- λ f : (t → u → v). (2)
        Lam $  -- λ x : u.             (1)
        Lam $  -- λ y : t.             (0)
               -- (f y) x
        (App (App two zero) one)
        

{-------------------------------------------------------------------------------

Part 2: Evaluation
==================

Of course, just having well-typed terms isn't all we want; we'd also like to be
able to interpret them as Haskell values.  We're going to follow a denotational
aproach: we'll translate terms in our language into terms in Haskell.  Our goal
is a function

    eval :: Expr '[] t -> t

That is to say: given a term that has type `t` with no free variable, we want to
interpret it as a Haskell value of type `t`.  However, we can't write such a
function directly---we wouldn't be able to interpret lambdas.  Instead, we need
to write a function with a type like

    eval :: Expr ts t -> Env ts -> t

where `Env ts` is a *value* environment that aligns with *typing* environment
*ts*; that is to say: it has to provide a value matching each type in `ts`.  We
can define such an environment as follows:

-------------------------------------------------------------------------------}

data Env :: [*] -> * where
    ENil  :: Env '[]
    ECons :: t -> Env ts -> Env (t : ts)

{-------------------------------------------------------------------------------

Environments are essentially lists of values, but where the types of values
match the types in the type of the environment.  For example:

>>> ECons 1 (ECons 'a' ENil) :: Env '[Int, Bool]
Couldn't match type ‘Char’ with ‘Bool’
Expected type: Env '[Int, Bool]
  Actual type: Env '[Int, Char]

Your final task is to write the aforementioned evaluation function.  We'll
break this down into two steps.  The first step is specific to variables: given
a variable of type `Var ts t` and a matching environment `Env ts`, we want to
produce a value of type `t`.

The intuition here is that the variable serves as an index into the environment:
variable `VZ` is the first element in the environment, variable `VS VZ` is the
next element, and so forth.

-------------------------------------------------------------------------------}

---------------------------------------------------------------------------------
-- Problems 3 and 4:
var :: Var ts t -> Env ts -> t
var VZ (ECons t _)      = t
var (VS v) (ECons t ts) = var v ts

{-------------------------------------------------------------------------------

Now, we can write the evaluation function itself.  We have four cases:

 * Variables are handled by the function above
 * Constants evaluate to themselves
 * Lambdas have to be interpreted as Haskell functions.  The key idea here is
   that you need to make a recursive call to `eval`, matching the environment
   that the body of the lambda expects.
 * As lambdas are interpreted as Haskell functions, application is interpreted
   as Haskell application.

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Problems 5-8:
eval :: Expr ts t -> Env ts -> t
eval (Var v) e   = var v e
eval (App m n) e = (eval m e) (eval n e)
eval (Lam t) e   = \x -> eval t (ECons x e)
eval (Const t) e = t

-- >>> eval tsnocE ENil 1 True
-- True

-- >>> eval (App tsnocE (Const 1)) ENil True
-- True

-- >>> eval (App (App tsnocE (Const  1)) (Const True)) ENil
-- True
