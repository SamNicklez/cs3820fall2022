{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Tests where



import Control.Monad (liftM2)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.List (intersperse)
-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems11

newtype CW w m a = CW {runCW :: m (a, w, Int)}

instance Functor m => Functor (CW w m) where
  fmap f (CW m) = CW (fmap (\(a, w, n) -> (f a, w, n)) m)

instance (Monoid w, Monad m) => Applicative (CW w m) where
  pure x = return x
  f <*> a = liftM2 ($) f a

instance (Monoid w, Monad m) => Monad (CW w m) where
  return x = CW (return (x, mempty, 0))
  CW m >>= k  = CW (m >>= (\(a, w, n) -> runCW (k a) >>= \ (b, v, m) ->
                                         return (b, mappend w v, n + m)))

instance (Monoid w, Monad m) => MonadWriter w (CW w m) where
  tell w = CW (return ((), w, 1))
  listen m = CW (runCW m >>= \(b, w, n) ->
                 return ((b, w), w, n))
  pass m = CW (runCW m >>= \((b, f), w, n) ->
               return (b, f w, n))

countWrites :: Monad m => CW w m a -> m Int
countWrites (CW m) = m >>= \(_, _, i) -> return i

--------------------------------------------------------------------------------

t1, t2 :: Test

t1 = test [ fst (runWriter (collatzStep 4)) == 2 @? "collatzStep 4 should return 2"
          , fst (runWriter (collatzStep 7)) == 22 @? "collatzStep 7 should return 22"
          , snd (runWriter (collatzStep 6)) == [6] @? "collatzStep 6 should announce 6"
          , snd (runWriter (collatzStep 14)) == [14] @? "collatzStep 14 should announce 14"
          , runIdentity (countWrites (collatzStep 12)) == 1 @? "collatzStep 12 should announce 1 value" ]

t2 = test ([ execWriter (collatz 5) ~?= [5,16,8,4,2,1]
           , execWriter (collatz 16) ~?= [16,8,4,2,1]
           , execWriter (collatz 11) ~?= [11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
           , runIdentity (countWrites (collatz 11)) ~?= 15 ]
           ++
           [ execWriter (collatz n) !! 4 ~?= fourSteps n | n <- [20..40], fourSteps n /= 1])
  where step 1 = 1
        step n | even n = n `div` 2
               | otherwise = 3 * n + 1
        fourSteps = step . step . step . step

t3, t4 :: Test

t3 = test [execState (increment i) j ~?= i + j | i <- [-5..5], j <- [-10..10]]

t4 = test [execState (decrement i) j ~?= j - i | i <- [-5..5], j <- [-10..10]]

t5, t6, t7, t8 :: Test

t5 = test [execState (runWriterT (command (Add i))) j ~?= i + j | i <- [0..10], j <- [5..20]]

t6 = test [execState (runWriterT (command (Subtract i))) j ~?= j - i | i <- [10..15], j <- [-5..5]]

t7 = test [execWriter (runStateT (command Write) j) ~?= [j] | j <- [-5..5]]

t8 = test [execWriter (runStateT (commands (Write : intersperse Write (map Add s) ++ [Write])) 0) ~?= scanl (+) 0 s | s <- [[0..n] | n <- [1..6]]]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests = [t1, t2, t3, t4, t5, t6, t7, t8]

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let tests = case read <$> (hd args) of
                Just x -> allTests !! (x - 1)
                Nothing -> test allTests
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
