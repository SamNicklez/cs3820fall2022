{-# LANGUAGE GADTs #-}
module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems7

--------------------------------------------------------------------------------
-- util
--------------------------------------------------------------------------------

err :: String -> String
err problem = problem ++ " is type-checking, not undefined, and not an infinite loop?\
                         \But you still have something wrong with it."
--------------------------------------------------------------------------------
-- p1
--------------------------------------------------------------------------------

pr1_1' :: Lte1 Zero Five
pr1_1' = pr1_1

pr1_2' :: Lte1 Three Five
pr1_2' = pr1_2

pr1_3' :: Lte1 Three Seven
pr1_3' = pr1_3

p1, p2, p3 :: Test
p1 = test [fin1 pr1_1' == () @? err "pr1_1"]

p2 = test [fin1 pr1_2' == () @? err "pr1_2"]

p3 = test [fin1 pr1_3' == () @? err "pr1_1"]

--------------------------------------------------------------------------------
-- p3
--------------------------------------------------------------------------------

data Shape = R | I | T Shape Shape
  deriving Eq

shape :: Lte2 m n -> Shape
shape Refl          = R
shape (Inst _)      = I
shape (Trans xs ys) = T (shape xs) (shape ys)

pr2_1a' :: Lte2 Zero Five
pr2_1a' = pr2_1a

pr2_1b' :: Lte2 Zero Five
pr2_1b' = pr2_1b

pr2_2' :: Lte2 Three Five
pr2_2' = pr2_2

pr2_3' :: Lte2 Three Seven
pr2_3' = pr2_3

p4, p5, p6, p7 :: Test
p4 = test [fin2 pr2_1a' == () @? err "pr2_1a"]

p5 = test [fin2 pr2_1b' == () @? err "pr2_1b",
           shape pr2_1a' /= shape pr2_1b'
              @? "pre3_1a and pre3_1b should have different implementations."]

p6 = test [fin2 pr2_2' == () @? err "pr2_2"]

p7 = test [fin2 pr2_3' == () @? err "pr2_3"]

allTests = [p1, p2, p3, p4, p5, p6, p7]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

argMap :: Int -> Test
argMap n
  | n <= 7 = test [allTests !! (n - 1)]
argMap _ = test allTests

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let tests = case read <$> (hd args) of
                Just x -> argMap x
                Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
