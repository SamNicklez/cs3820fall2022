module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems8

--------------------------------------------------------------------------------
-- tests

err :: String -> String
err problem = problem ++ " is type-checking, not undefined, and not an infinite loop?\
                         \But you still have something wrong with it."

check :: Proof p => String -> p -> Test
check s p = test [finite p == () @? err s]

--------------------------------------------------------------------------------

t1, t2, t3, t4 :: Test

p1' :: Rtc Lt1 Three Five
p1' = p1

t1 = check "p1" p1'

p2' :: Rtc Gt1 Five Three
p2' = p2

t2 = check "p2" p2'

p3' :: Star Lt1 Three Five
p3' = p3

t3 = check "p3" p3'

p4' :: Star Gt1 Five Three
p4' = p4

t4 = check "p4" p4'

--------------------------------------------------------------------------------

t5, t6, t7, t8 :: Test

p5a :: Rtc Gt1 Five Three
p5a = convRtc p1'

p5b :: Rtc Gt1 Four Two
p5b = convRtc (Trans (Trans Refl (Inst Lt1)) (Trans (Inst Lt1) Refl))

t5 = test [check "convRtc" p5a, check "convRtc" p5b]

p6a :: Rtc Lt1 Three Five
p6a = starToRtc p3'

p6b :: Rtc Gt1 Five Three
p6b = starToRtc p4'

t6 = test [check "starToRtc" p6a, check "starToRtc" p6b]

p7a :: Star Lt1 Three Five
p7a = rtcToStar p1'

p7b :: Star Gt1 Five Three
p7b = rtcToStar p2'

p7c :: Star Lt1 Two Four
p7c = rtcToStar (Trans (Trans Refl (Inst Lt1)) (Trans (Inst Lt1) Refl))

t7 = test [check "rtcToStar" p7a, check "rtcToStar" p7b, check "rtcToStar" p7c]

p8a :: Star Gt1 Five Three
p8a = convStar p3'

p8b :: Star Gt1 Four Two
p8b = convStar (Step Lt1 (Step Lt1 Done))

t8 = test [check "convStar" p8a, check "convStar" p8b]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

tests = [t1, t2, t3, t4, t5, t6, t7, t8]

argMap :: Int -> Test
argMap n
  | n <= 8 = tests !! (n - 1)
  | otherwise = test tests

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
