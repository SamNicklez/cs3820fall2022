{-# LANGUAGE FlexibleContexts #-}
module Problems11 where

import Control.Monad.Writer
import Control.Monad.State

announce :: MonadWriter [a] m => a -> m ()
announce = tell . (:[])

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 11
================================

This problem set will explore some basic programming with monads.  In
particular, we're going to look at two classes of side-effects.

* The MonadWriter class captures the side effect of generating (or logging)
  additional results during computation.

* The MonadState class captures the side effect of reading and writing to a
  single state value during computation.

Each of your assignments will be *polymorphic* in the resulting monad.  That is
to say: they will apply in *any* monad which provides the necessary operations.
This means that the type system will *require* you to use the monad operations
(return, >>=, and the MonadWriter or MonadState) operations.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part 1: MonadWriter
-------------------

The MonadWriter class captures the side effect of generating results during
computation.  The most common instance of this pattern is *logging*; for
example, a web server might generate a log of all requests it serves at the same
time that it serves those requests.

The MonadWriter class itself has two parameters (so it appears as `MonadWriter w
m`).  The first (`w`) is the type of the results generated along the way.  The
second (`m`) is the type of the monad itself.  So a type like

    MonadWriter [Int] m => Int -> m Int

describes a computation which takes an `Int` input, returns an `Int` output, and
produces an additional `[Int]` shaped result along the way.  We're *always*
going to have lists as the generated results, although `MonadWriter` will
actually work for any `Monoid`.

We'll start by reducing `MonadWriter` to a single operation

    announce :: MonadWriter [a] m => a -> m ()

If `x :: a`, then `announce x` adds `a` to the additional results.  It has
nothing to return---that is, it's an operation done *purely* for its
side-effects---so it returns the unit type `()`.

-------------------------------------------------------------------------------}

-- For example, here's a function that announces an increasing list of numbers:

announceIncreasing :: MonadWriter [Int] m => Int -> Int -> m ()
announceIncreasing m 0 = announce m
announceIncreasing m n = announce m >>= \ () ->
                         announceIncreasing (m + 1) (n - 1)

-- >>> runWriter (announceIncreasing 4 6)
-- ((),[4,5,6,7,8,9,10])

---------------------------------------------------------------------------------
-- Problem 1: collatzStep
--
-- You may remember the Collatz sequence from PS2.  The Collatz sequence
-- is defined by:
--
--   1. If the current number n is 1, the sequence ends
--   2. If the current number n is even, the sequence continues with n/2
--   3. If the current number n is odd, the sequence continues with 3n+1
--
-- For example, the Collatz sequence starting from 4 is 4,2,1; the Collatz
-- sequence starting from 5 is 5,16,8,4,2,1.
--
-- For this problem, you'll write a function that computes a *single step* of
-- the Collatz function, announcing its result before it does so.  For example,
-- if the input is 3, your function should announce 3 and return 10.

collatzStep :: MonadWriter [Int] m => Int -> m Int
collatzStep n    
    | n == 1 = return 1
    | even n = announce n >> return (n `div` 2)
    | odd n = announce n >>return ( 3 *n+1)

-- >>> runWriter (collatzStep 4)
-- (2,[4])

-- >>> runWriter (collatzStep 7)
-- (22,[7])

--------------------------------------------------------------------------------
-- Problem 2: collatz
--
-- Next, you'll use your `collatzStep` function to write a function that
-- announces the entire Collatz sequence from a given starting point.

collatz :: MonadWriter [Int] m => Int -> m ()
collatz 1 = announce 1
collatz m
  | even m = collatzStep m >> collatz (m `div` 2)
  | odd m = collatzStep m >> collatz (3 * m + 1)

-- >>> runWriter (collatz 4)
-- ((),[4,2,1])

-- >>> runWriter (collatz 3)
-- ((),[3,10,5,16,8,4,2,1])

-- >>> runWriter (collatz 5)
-- ((),[5,16,8,4,2,1])

{-------------------------------------------------------------------------------

Part 2: MonadState
------------------

MonadState captures the side effect of mutable state---that is to say,
computations that may read and write some persistent value over the course of
the computation.  (This isn't quite the same as having mutable variables yet...
it's more like having mutable variable.  Although, of course, you can use it to
simulate arbitrarily many variables.)

Like MonadWriter, the MonadState class itself has two parameters---the type of
the state, and the type of the monad itself.  So a type like

    MonadState Int m => Char -> m Char

describes a computation which takes a character input, returns a character
result, and may access or change some integer state on the way.

For MonadState, we have two essential operations

    get :: MonadState s m => m s
    put :: MonadState s m => s -> m ()

The `get` operation returns the current value of the state; `put x` updates the
value of the state to be `x`.

-------------------------------------------------------------------------------}

-- For example, here's a function that doubles the current state
doubleState :: MonadState Int m => m ()
doubleState = get >>= \x ->
              put (2 * x)

-- >>> runState doubleState 4
-- ((),8)

-- >>> runState doubleState (-2)
-- ((),-4)

---------------------------------------------------------------------------------
-- Problems 3 and 4: increment and decrement
--
-- Write functions that increment or decrement the current state by the
-- specified amount.

increment, decrement :: MonadState Int m => Int -> m ()
increment x = get >>= \y ->
              put (y + x)

decrement x = get >>= \y ->
              put (y - x)

--------------------------------------------------------------------------------
-- Problems 5-8: the desktop adding machine
--
-- A desktop adding machine combines a simple calculator with a paper output of
-- its calculations.  We'll simulate this using `MonadState` and `MonadWriter`.
-- The inputs to our desktop calculator take one of three forms:

data Command = Add Int | Subtract Int | Write

-- Which mean:
--
--   * Add n:         adds n to the running total
--   * Subtract n:    subtracts n from the running total
--   * Write:         writes the current total to the tape
--
-- Interpreting a series of these operations requires *both* MonadState (to
-- store the running total) and MonadWriter (to generate the tape).
--
-- For example, a sequence of commands like
--
--    [Add 1, Add 4, Write, Add 3, Write, Subtract 20]
--
-- if we start with running total 0, would end with running total -12 and a tape
-- containing 5 and 8.

-- Problems 5-7: write a function that interprets a single command
command :: (MonadState Int m, MonadWriter [Int] m) => Command -> m ()
command (Add n) = get >>= \y -> put (y + n)
command (Subtract n) = get >>= \y -> put (y - n)
command Write = get >>= \y -> announce y

-- >>> runState (runWriterT (command (Add 10))) 4
-- (((),[]),14)

-- >>> runState (runWriterT (command (Subtract 4))) 9
-- (((),[]),5)

-- >>> runState (runWriterT (command (Write))) 20
-- (((),[20]),20)

-- Problem 8: use your `command` function to implement a series of commnands.
commands :: (MonadState Int m, MonadWriter [Int] m) => [Command] -> m ()
commands (c:cs) = command c >> commands cs
commands [] = command (Add 0)


{-collatz :: MonadWriter [Int] m => Int -> m ()
collatz 1 = announce 1
collatz m
  | even m = collatzStep m >> collatz (m `div` 2)
  | odd m = collatzStep m >> collatz (3 * m + 1)-}

-- >>> runState (runWriterT (commands [Add 1, Write, Subtract 1, Write])) 0
-- (((),[1,0]),0)

-- >>> runState (runWriterT (commands [Add 5, Add 5, Write, Add 6])) (-10)
-- (((),[0]),6)
