module Tests where

import Prelude hiding (negate)

-- GHC
import System.Exit
import System.Environment

-- Base
import Data.List (sort)

-- External
import Test.HUnit

-- Lib
import Problems4

--------------------------------------------------------------------------------
-- Problems 1-6
--------------------------------------------------------------------------------

tests1 = test $
  [ variable "x" ~?= variable "x"
  , variable "X" ~?= variable "X"
  ]

tests2 = test $
  [ truth ~?= truth
  , TestCase . assertBool "" $
      truth /= variable "x"
  , TestCase . assertBool "" $
      truth /= variable "Foo"
  ]

tests3 = test $
  [ falsity ~?= falsity
  , TestCase . assertBool "" $
      falsity /= truth
  , TestCase . assertBool "" $
      falsity /= variable "x"
  ]

tests4 = test $
  [ negate truth   ~?= negate truth
  , negate falsity ~?= negate falsity
  , negate (variable "x") ~?= negate (variable "x")
  , TestCase . assertBool "" $
      negate truth /= truth
  , TestCase . assertBool "" $
      negate falsity /= falsity
  , TestCase . assertBool "" $
      negate (variable "x") /= variable "x"
  ]

tests5 = test $
  [ conj [] ~?= truth
  , conj [variable "x"] ~?= variable "x"
  , conj [truth]        ~?= truth
  , TestCase . assertBool "" $
      conj [variable "x", variable "y"] /= truth
  , TestCase . assertBool "" $
      conj [variable "y", variable "x"] /= falsity
  , TestCase . assertBool "" $
      conj [variable "z"] /= negate (variable "z")
  , conj [variable "x", variable "y", variable "z"]
    ~?= conj [variable "x", conj [variable "y", variable "z"]]
  , conj [variable "x", variable "y", variable "z", variable "a"]
    ~?= conj [variable "x", conj [variable "y", conj [variable "z", variable "a"]]]
  , (TestCase . assertBool "No left association" $
      conj [conj [variable "x", variable "y"], variable "z"]
      /= conj [variable "x", variable "y", variable "z"])
  ]

tests6 = test $
  [ disj [] ~?= falsity
  , disj [variable "x"] ~?= variable "x"
  , disj [falsity]      ~?= falsity
  , TestCase . assertBool "" $
      disj [variable "x", variable "y"] /= truth
  , TestCase . assertBool "" $
      disj [variable "y", variable "x"] /= falsity
  , TestCase . assertBool "" $
      disj [variable "z"] /= negate (variable "z")
  , TestCase . assertBool "" $
       disj [variable "x", variable "y"] /= conj [negate (variable "x"), negate (variable "y")]
  , TestCase . assertBool "" $
       disj [negate (variable "x"), negate (variable "y")] /= conj [variable "x", variable "y"]       
  , disj [variable "x", variable "y", variable "z"]
    ~?= disj [variable "x", disj [variable "y", variable "z"]]
  , disj [variable "x", variable "y", variable "z", variable "a"]
    ~?= disj [variable "x", disj [variable "y", disj [variable "z", variable "a"]]]
  , (TestCase . assertBool "No left association" $
      disj [disj [variable "x", variable "y"], variable "z"]
      /= disj [variable "x", variable "y", variable "z"])
  ]

--------------------------------------------------------------------------------
-- Problems 7-12
--------------------------------------------------------------------------------

vars1 = map (: []) ['a'..'z']
vars2 = map (: []) ['A'..'Z']

tests7 = test $
  [ eval (variable "x") []    ~?= False
  , eval (variable "x") ["x"] ~?= True
  , eval (variable "doctor who") ["doctor what", "doctor where", "doctor who"] ~?= True
  , eval (variable "X") vars1 ~?= False
  , eval (variable "x") vars1 ~?= True
  ]

tests8 = test $
  [ eval truth   []    ~?= True
  , eval truth   vars1 ~?= True
  ]

tests9 = test $
  [ eval falsity []    ~?= False
  , eval falsity vars1 ~?= False
  ]

tests10 = test $
  [ eval (negate truth)          []     ~?= False
  , eval (negate truth)          vars1  ~?= False
  , eval (negate falsity)        []    ~?= True
  , eval (negate falsity)        vars1 ~?= True
  , eval (negate (variable "x")) vars2 ~?= True
  , eval (negate (variable "x")) vars1 ~?= False
  ]

tests11 = test $
  [ eval (conj [])        []           ~?= True
  , eval (conj [falsity]) []           ~?= False
  , eval e1               []           ~?= True
  , eval e1               vars1        ~?= True
  , eval e2               []           ~?= False
  , eval e3               (init vars1) ~?= False
  , eval e3               vars1        ~?= True
  , eval e4               []           ~?= False
  , eval e4               ["x"]        ~?= True
  ]
  where
  e1 = conj [truth, truth]
  e2 = conj [e1, falsity]
  e3 = conj (map variable vars1)
  e4 = negate (conj (map (negate . variable) vars1))

tests12 = test $
  [ eval (disj [])      []           ~?= False
  , eval (disj [truth]) []           ~?= True
  , eval e1             vars1        ~?= False
  , eval e2             []           ~?= True
  , eval e3             vars2        ~?= False
  , eval e3             [last vars1] ~?= True
  , eval e4             vars1        ~?= True
  , eval e4             (init vars1) ~?= False
  ]
  where
  e1 = disj [falsity , falsity]
  e2 = disj [e1, truth]
  e3 = disj (map variable vars1)
  e4 = negate . disj . map (negate . variable) $ vars1

--------------------------------------------------------------------------------
-- Problems 13-15
--------------------------------------------------------------------------------

tests13 = test $
  [       variables truth   ~?= []
  ,       variables falsity ~?= []
  , sort (variables e1)     ~?= vars1
  , sort (variables e2)     ~?= sort (vars1 ++ vars2)
  , sort (variables e3)     ~?= vars1
  ]
  where
  e1 = disj . map variable $ vars1
  e2 = conj [(disj . map variable $ vars2), e1]
  e3 = negate (conj [e1, e1])

tests14 = test $
  [ subsets []                     ~?= [[]]
  , sortt (subsets ["a"])          ~?= sortt [[],["a"]]
  , sortt (subsets (take 3 vars1))
    ~?= sortt [[],["a"],["b"],["c"], ["a","b"], ["c","a"], ["b","c"], ["a","b","c"]]
  , sortt (subsets (take 4 vars2))
    ~?= sortt [[],["A"],["B"],["C"], ["D"]
              , ["A","B"], ["A","C"], ["B","C"], ["A", "D"], ["B", "D"], ["D", "C"]
              , ["A","B","C"], ["A", "B", "D"], ["B", "C", "D"], ["A", "C", "D"]
              , ["A", "B", "C", "D"]]
  ]
  where
  sortt = sort . map sort

tests15 = test $
  [ unsatisfiable truth          ~?= False
  , unsatisfiable falsity        ~?= True
  , unsatisfiable (variable "x") ~?= False
  , unsatisfiable e1             ~?= False
  , unsatisfiable e2             ~?= False
  , unsatisfiable e3             ~?= True
  , unsatisfiable e4             ~?= True
  ]
  where
  e1 = disj . map variable $ vars1
  e2 = conj . map variable $ (take 10 vars1)
  e3 = conj [variable "a", negate (variable "a")]
  e4 = conj [(disj . map variable . take 10 $ vars1)
            , (conj . map (negate . variable) . take 10 $ vars1)]
  

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests :: [Test]
allTests = [ tests1 , tests2 , tests3 , tests4 , tests5 , tests6
           , tests7 , tests8 , tests9 , tests10, tests11, tests12
           , tests13, tests14, tests15
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
