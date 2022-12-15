module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems6 (Formula (..), distribute, coeff, poly)

--------------------------------------------------------------------------------
-- -1. (infrastructure)
--------------------------------------------------------------------------------

distributed :: Formula -> Bool
distributed = go 0 where
    go _ X         = True
    go _ (C _)     = True
    go n (e :+: f) = n < 1 && go 0 e && go 0 f
    go n (e :*: f) = n < 2 && go 1 e && go 1 f
    go n (e :^: _) = n < 2 && go 2 e

distributes :: Formula -> Bool
distributes = distributed . distribute

--------------------------------------------------------------------------------
-- 0. (definitions)
--------------------------------------------------------------------------------

one = C 1
two = C 2
three = C 3
four = C 4

x = X

neg :: Formula -> Formula
neg (C n) = C (-n)
neg _ = error "no"

--------------------------------------------------------------------------------
-- 1. Multiplication over addition
--------------------------------------------------------------------------------

tests1 = test $ [
    distributes (one :*: (two :+: three))  ~? "1 * (2 * 3) is distributed."
  , distributes (x :*: (two :+: three)) ~? "x * (2 + 3) distributes"
  , distributes ((two :+: three) :*: x) ~? "(2 + 3) * x distributes"

  -- Ambiguity -- x^2 could also be represented as x * x...
  , distributes ((x :+: two) :*: (x :+: three)) ~? "(x + 2) * (x + 3) is distributed"
  , distributes (x :*: (x :+: three)) ~? "x * (x + 3) distributes"
  , distributes ((x :+: three) :*: x) ~? "(x + 3) * x distributes"
  -- nested example(s)
  , distributes ((x :+: (two :+: x)) :*: (x :*: three)) ~? "(x + (2 + x)) * (x * 3) is distributed"
  ]

--------------------------------------------------------------------------------
-- 2. exponentiation over addition
--------------------------------------------------------------------------------

tests2 = test $ [
    distribute (x :^: 0) ~?= one
    -- (x + n)¹ could be represented as
    --   (x + n)¹ * 1
    -- or
    --  (x + n)¹    
  , distributes ((x :+: two) :^: 1) ~? "(x + 2)^1 distributes"
  , distributes ((x :+: two) :^: 2)   ~? "(x + 2)^2 distributes"
  , distributes ((x :+: two) :^: 4)   ~? "(x + 2)^4 distributes"
  , distributes ((two :+: x) :^: 4)   ~? "(2 + x)^4 distributes"
  , distributes ((two :+: four) :^: 4)   ~? "(2 + 4)^4 distributes"
  -- nested example(s)
  , distributes (((x :+: two) :+: (x :+: four)) :^: 5) ~? "((x + 2) + (x + 4))^5 distributes"
  ]

--------------------------------------------------------------------------------
-- 3. exponentiation over multiplication
--------------------------------------------------------------------------------

tests3 = test $ [
    distributes ((x :*: two) :^: 1) ~? "(x * 2)^1 distributes"
  , distributes ((x :*: two) :^: 2)   ~? "(x * 2)^2 distributes"
  , distributes ((x :*: two) :^: 4)   ~? "(x * 2)^4 distributes"
  , distributes ((two :*: x) :^: 4)   ~? "(2 * x)^4 distributes"
  -- nested example(s)
  , distributes (((two :*: x) :*: (x :*: four)) :^: 3) ~? "((2 * x) * (x * 4))^3 distributes"
  
  ]

--------------------------------------------------------------------------------
-- 4. exponentiation over exponentiation
--------------------------------------------------------------------------------

tests4 = test $
    -- constant example -- can either be represented as 2⁸ or 256...
  [distributes ((two :^: 2) :^: 4) ~? "2⁸ distributes"]
  ++
  -- do not face canonicity issues here because we are distributing m, n :: Int
  [distribute ((x :^: n) :^: m) ~?= (x :^: (n * m)) | n <- [1..5], m <- [5..10]]
  ++
  -- nested example
  [
    let p = n + 1 in
    distribute (((x :^: n) :^: m) :^: p) ~?= (x :^: (n * m * p)) | n <- [1..5], m <- [5..10]
  ]

--------------------------------------------------------------------------------
-- 5-7. `coeff`
--------------------------------------------------------------------------------

-- base cases
tests5 = test $ [
    -- constant
    coeff one ~?= (1, 0)
  , coeff four ~?= (4, 0)
    -- just var
  , coeff x ~?= (1, 1)
    -- var with coefficient
  , coeff (x :*: two) ~?= (2, 1)
  -- constant multiplication
  , coeff (three :*: four) ~?= (12, 0)
  , coeff (two :*: three :*: four) ~?= (24, 0)
  ]

-- multiplication
tests6 = test $ [
      coeff (x :*: x) ~?= (1, 2)
    , coeff (x :*: x :*: one) ~?= (1, 2)
    , coeff (x :*: x :*: two) ~?= (2, 2)
    , coeff (x :*: three :*: x :*: two :*: x) ~?= (6, 3)
  ]

-- exponentiation
tests7 = test $ [
    coeff (x :^: 2) ~?= (1, 2)
  , coeff (x :^: 10) ~?= (1, 10)
  -- multiplication and exponentiation
  , coeff (x :*: x :^: 2) ~?= (1, 3)
  , coeff (x :^: 10 :*: two :*: x :^: 15) ~?= (2, 25)
  ]

--------------------------------------------------------------------------------
-- 8-10. `poly`
--------------------------------------------------------------------------------

-- tests for arithmetic expressions that have already been distributed and have
-- at most one summand of each power
tests8 = test $
  [ poly (C 4 :*: C 2) ~?= [8]
  , poly (x :^: 1)     ~?= [0, 1]
  , poly (C 100 :*: x :^: 42) ~?= replicate 42 0 ++ [100]
  , poly e0            ~?= [6,0,0,0,0,5,0,0,0,0,12]
  , poly e1            ~?= 10 : [100,99..1]
  ]
  where
  e0 :: Formula
  e0 = (C 2 :*: C 3) :+: (C 5 :*: C 1 :*: x :^: 5) :+: (C 4 :*: C 3 :*: x :^: 10)

  e1 :: Formula
  e1 = foldr (\ p f -> f :+: (C . fst $ p) :*: (x :^: snd p)) (C 10) $ zip [100,99..1] [1..100]

-- tests for arithmetic expressions that have already been distributed and have
-- multiple summands of each power
tests9 = test $
  [ poly (C 4 :+: C 7) ~?= [11]
  , poly e0            ~?= [4,55,0,0,0,0,0,100]
  , poly e1            ~?= 10 : replicate 50 101
  ]
  where
  e0 :: Formula
  e0 = foldr (\ p f -> C (fst p) :*: X :^: 1 :+: C (snd p) :*: X :^: 7 :+: f) (C 4) $ zip [1..10] [1,3..19]

  e1 :: Formula
  e1 = foldr (\ p f -> f :+: (C . fst $ p) :*: (x :^: snd p)) (C 10) $ zip [100,99..1] ([1..50] ++ [50,49..1])

-- tests for arithmetic expressions that have not been distributed
tests10 = test $
  [ poly ((C 3 :*: x :+: C 2) :^: 3)           ~?= [8,36,54,27]
  , poly ((C 2 :*: x :+: C 3) :*: (C 1 :+: x)) ~?= [3,5,2]
  , poly (x :+: x :+: (C 2 :*: x))             ~?= [0,4]
  , poly (e0 :^: 2)                            ~?= [16,440,3025,0,0,0,0,800,11000,0,0,0,0,0,10000]
  , poly e1                                    ~?= [2,4,4,2]
  , poly (e1 :*: e0)                           ~?= [8,126,236,228,110,0,0,200,400,400,200]
  , poly (C (-2) :*: x :*: e1)                 ~?= [0,-4,-8,-8,-4]
  ]
  where
  e0 = foldr (\ p f -> C (fst p) :*: X :^: 1 :+: C (snd p) :*: X :^: 7 :+: f) (C 4) $ zip [1..10] [1,3..19]

  e1 = (x :+: x :+: (C 2 :*: x :*: x :+: C 2)) :*: (C 1 :+: x)
--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------


allTests :: [Test]
allTests = [ tests1 , tests2 , tests3 , tests4 , tests5 , tests6
           , tests7 , tests8 , tests9, tests10
           ]

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

