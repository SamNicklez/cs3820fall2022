module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems2

--------------------------------------------------------------------------------
-- Tests 1

tests1, tests2, tests3, tests4, tests5, tests6 :: Test

tests1 = test $
  -- simple tests
  [ semifact 0 ~?= 1
  , semifact 1 ~?= 1
  , semifact 2 ~?= 2
  , semifact 3 ~?= 3
  , semifact 4 ~?= 8
  ]
  ++
-- Some facts about !!, ∀ n ∈ N:
--    n!! ≡ (n - 2)!! mod n
  [semifact n `mod` semifact (n - 2) ~?= 0 | n <- [1..15]]
  ++
--     n!!
--   –—————— = n
--  (n - 2)!!
  [semifact n `div` (semifact (n - 2)) ~?= n | n <- [1..10]]
  ++
--  n!! * (n - 1)!! = n!
  [semifact 7 * semifact 6 ~?= 7 * 6 * 5 * 4 * 3 * 2]


--------------------------------------------------------------------------------
-- Tests 2

tests2 = test $
  [ last (collatz x) @?= 1 | x <- [15..20]] ++
  [ head (collatz x) @?= x | x <- [35..40]] ++
  [ take 5 (collatz 50) @?= [50, 25, 76, 38, 19]]

--------------------------------------------------------------------------------
-- Tests 3

tests3 = test
  [ oddIndexes "Curabitur lacinia pulvinar nibh" ~?= "Crbtrlcnaplia ih"
  , oddIndexes n                                 ~?= n
  , oddIndexes [()]                              ~?= [()]
  , oddIndexes [1..100]                          ~?= [1,3..99]]
  where
  n :: [Bool]
  n = []

--------------------------------------------------------------------------------
-- Tests 4

tests4 = test
  [ alternating []                ~?= 0
  , alternating [1337]            ~?= 1337
  , alternating [100, 50, 25, 12] ~?= 100 - 50 + 25 - 12
  , alternating [1..100]          ~?= -50
  , alternating [(-25)..(-1)]     ~?= -13]

--------------------------------------------------------------------------------
-- Tests 5

tests5 = test
  [ checkValidSimple "1204412317" ~?= True
  , checkValidSimple "2992014129" ~?= False
  , checkValidSimple "505"        ~?= True
  , checkValidSimple "607"        ~?= False
  , checkValidSimple nines        ~?= True
  , checkValidSimple sevens       ~?= True
  ]
  where
  nines  = (++ "89") . take 21 . repeat $ '9'
  sevens = (++ "19") . take 17 . repeat $ '7'

--------------------------------------------------------------------------------
-- Tests 6

tests6 = test
  [ checkValid "5105105105105100" ~?= True
  , checkValid "5101505105105100" ~?= False
  , checkValid "3530111333300000" ~?= True
  , checkValid "3531011333300000" ~?= False
  , checkValid "6011000990139424" ~?= True
  , checkValid "1111111111111111" ~?= False]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests :: [Test]
allTests =
  [ tests1 , tests2 , tests3 , tests4 , tests5 , tests6 ]

argMap :: Int -> Test
argMap n
  | n > 0 && n <= length allTests = allTests !! (n - 1)
  | otherwise                     = test allTests

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
