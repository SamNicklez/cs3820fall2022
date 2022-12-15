module Tests where

-- GHC
import System.Exit
import System.Environment

import Prelude hiding ( even, curry, uncurry)

-- External
import Test.HUnit

-- Lib
import Problems1
import GHC.Base (undefined)
import Text.Read (Lexeme(String))

err :: String -> String
err problem = ""
--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------


tests1, tests2, tests3, tests4 :: Test
tests1 = test [
  -- double
  double 0           @?= 0,
  double 1           @?= 2,
  double 2           @?= 4,
  double (2 * 12345) @?= ((1 + 1 + 2) * 12345),
  double (-1337)     @?= -2674
  ]

tests2 = test [
  -- doubleProd
  doubleProd 0 1     @?= 0,
  doubleProd 1 2     @?= 8,
  doubleProd 3 4     @?= 48,
  doubleProd 20 40   @?= 2^5 * 100,
  doubleProd (-7) 13 @?= (-14) * 26
  ]

tests3 = test [
  -- even
  even 10            @?= True,
  even 1             @?= False,
  even (2^10 :: Int) @?= True,
  even (3^12 :: Int) @?= False
  ]

tests4 = test [
  -- step
  step 1 10          @?= 1,
  step 12 24         @?= 1,
  step 1 0           @?= 0,
  step (-20) (-30)   @?= 0
  ]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------
tests5, tests6, tests7, tests8 :: Test
tests5 = test [
  -- pairSum
  pairSum (1, 2) @?= 3,
  pairSum (3, 4) @?= 7,
  pairSum
      (pairSum (pairSum (pairSum (0, 1000), 200), 30), 4)
      @?= 1234
  ]

tests6 = test [
  -- prodSum
  prodSum (1, 2) 3 @?= 5,
  prodSum (3, 4) 1 @?= pairSum (3, 4),
  prodSum (10, 1) 40 @?= 401
  ]


assertUnequal :: (Show a, Eq a) => a -> a -> Assertion
assertUnequal expected actual = assertBool ("expected " ++ show actual ++ " to differ from " ++ show actual) (expected /= actual)

tests7 =  test $
  -- selfInv
  [ (selfInv . selfInv $ (x, y)) @?= (x, y) | x <- [1..15], let y = x + 1]
  ++
  [ assertUnequal (x, y) (selfInv (x, y)) | x <- [1..15], let y = x + 1 ]

tests8 = test $
  -- selfInv'
  [ (selfInv' . selfInv' $ (x, y)) @?= (x, y) | x <- [1..15], let y = x + 1]
  ++
  [ assertUnequal (x, y) (selfInv' (x, y)) | x <- [1..15], let y = x + 1 ]
  ++
  [ (selfInv' . selfInv' $ (x, y)) @?= (x, y) | x <- [True,False], y <- [True, False]]
  ++
  [ assertUnequal (x, y) (selfInv' (x, y)) | x <- [True, False], y <- [True, False], x /= y]

--------------------------------------------------------------------------------
-- Part 3
--------------------------------------------------------------------------------
tests9, tests10, tests11, tests12 :: Test

-- pipe
tests9 = test
  [ pipe show length 1337                    @?= 4
  , pipe (* 2) (pipe (`mod` 2) (== 0)) 43    @?= True
  , pipe (const "Hi there") (const "Bye!") 0 @?= "Bye!"
  ]

-- applyFst
tests10 = test
  [ applyFst show (True,0)                  @?= ("True", 0)
  , applyFst fst (('H', [True]) , "askell") @?= ('H', "askell")
  , applyFst (map show) ([1,2,3], [4,5,6])  @?= (["1", "2", "3"], [4,5,6])
  ]

-- curry
tests11 = test
  [ curry fst True False                   @?= True
  , curry (\ x -> fst x + snd x) 1.1 2.2   @?= 1.1 + 2.2
  , curry (\ x -> (snd x , fst x)) 'a' 'c' @?= ('c', 'a')
  ]

tests12 = test
  [ uncurry const (True, False)   @?= True
  , uncurry (+) (1.1, 2.2)        @?= 1.1 + 2.2
  , uncurry (flip (,)) ('a', 'c') @?= ('c', 'a')
  , (curry . uncurry) (*) 3 7     @?= 21
  ]

--------------------------------------------------------------------------------
-- Part 4
--------------------------------------------------------------------------------
tests13, tests14, tests15, tests16 :: Test

-- doubled
tests13 = test
  [ doubled []                 @?= []
  , doubled [1..10]            @?= [2,4..20]
  , doubled [1337, 525600, 23] @?= map (* 2) [1337, 525600, 23]
  , doubled [-25 .. -1]        @?= map (* 2) [-25 .. -1]
  ]

-- evens
tests14 = test
  [ evens [1..100]    @?= [2,4..100]
  , evens []          @?= []
  , evens [-25 .. -1] @?= [-24, -22 .. -2]
  , evens [1]         @?= []
  ]

-- countPositive
tests15 = test
  [ countPositive []                   @?= 0
  , countPositive [0, -1 .. -20]       @?= 0
  , countPositive [-25..25]            @?= 25
  , countPositive [-3820, 3802, -3820] @?= 1
  ]

-- average
tests16 = test
  [ average [] == average []          @?= False -- NaN != NaN
  , average [1]                       @?= 1
  , average [-2, 0, 11]               @?= 3
  , average [13, 17, 19, 23, 29]      @?= 20.2
  , average [-31, -37, -41, -43, -47] @?= -39.8
  ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests :: [Test]
allTests = [ tests1, tests2, tests3, tests4
           , tests5, tests6, tests7, tests8
           , tests9, tests10, tests11, tests12
           , tests13, tests14, tests15, tests16 ]

argMap :: Int -> Test
argMap n
  | n > 0 && n <= 16 = allTests !! (n - 1)
  | otherwise        = test allTests

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
