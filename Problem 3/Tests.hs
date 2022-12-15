{-# LANGUAGE OverloadedStrings #-}
module Tests where

-- GHC
import System.Exit
import System.Environment

-- Base
import Data.Char (toLower, ord, chr)
import Data.List (sort, sortBy)

-- External
import Test.HUnit

-- Lib
import Problems3

--------------------------------------------------------------------------------
-- 1
--------------------------------------------------------------------------------

tests1 = test $
  -- split tests
  [ split id id 1 ~?= (1, 1)
  , split fst snd (1, 2) ~?= (1, 2)
  , split head tail "foo" ~?= ('f', "oo")
  , split (split head tail) (split tail head) [1.0, 2.0] ~?= ((1.0, [2.0]), ([2.0], 1.0))
  , split (fmap (+1)) (fmap (*2)) [1..10] ~?= ([2..11], [2 * x | x <- [1..10]])
  ]
  
--------------------------------------------------------------------------------
-- 2
--------------------------------------------------------------------------------

tests2 = test $
  -- times tests
  [ times id id (1, 1) ~?= (1, 1)
  , times fst snd ((1, 2), (1, 2)) ~?= (1, 2)
  , times head tail ("foo", "bar") ~?= ('f', "ar")
  , times (fmap (+1)) (fmap (*2)) ([1..10], [5..10]) ~?= ([2..11], [2 * x | x <- [5..10]])
  ]
  
--------------------------------------------------------------------------------
-- 3
--------------------------------------------------------------------------------

nestTuple1 :: ((a, b), c) -> (((a, b), c), ((a, b), c))
nestTuple1 t = (t, t)
nestTuple2 = nestTuple1 . nestTuple1
nestTuple4 = nestTuple2 . nestTuple2

tests3 = test $
  
  [
    -- leftAssoc
    leftAssoc (1, (2, 3)) ~?= ((1, 2), 3)
  , leftAssoc (('a', 'b'), (('c', 'd'), ('e', 'f'))) ~?= ((('a', 'b'), ('c', 'd')), ('e', 'f'))
  -- rightAssoc
  , rightAssoc ((1, 2), 3) ~?= (1, (2, 3))
  , rightAssoc ((('a', 'b'), ('c', 'd')), ('e', 'f')) ~?= (('a', 'b'), (('c', 'd'), ('e', 'f')))
  -- these two should be (left and right) inverse regardless of the input
  , (leftAssoc  . rightAssoc . nestTuple4  $ ((1, 2), 3)) ~?= nestTuple4 ((1, 2), 3)
  , (rightAssoc . leftAssoc  . nestTuple4  $ ((1, 2), 3)) ~?= nestTuple4 ((1, 2), 3)
  ]

--------------------------------------------------------------------------------
-- 4
--------------------------------------------------------------------------------

tests4 = test $
  
  [
    maybeDefault 1 Nothing ~?= 1
  , maybeDefault True (Just False) ~?= False
  , maybeDefault (Just False) Nothing ~?= Just False
  , maybeDefault 1.0 (Just 2.0) ~?= 2.0
  ]


--------------------------------------------------------------------------------
-- 5
--------------------------------------------------------------------------------
tests5 = test $
  
  [
    maybeBranch (+1) 1 Nothing ~?= 1
  , maybeBranch not True (Just False) ~?= True
  , maybeBranch (fmap not) (Just False) Nothing ~?= Just False
  , maybeBranch (*2) 1.0 (Just 2.0) ~?= 4.0
  ]

--------------------------------------------------------------------------------
-- 6
--------------------------------------------------------------------------------

tests6 = test $
  
  [
    maybeMap (+1) (Just 1) ~?= Just 2
  , maybeMap not (Just False) ~?= Just True
  , maybeMap (&& True) (Just True) ~?= Just True
  , maybeMap (*2) (Just 2.0) ~?= Just 4.0
  , maybeMap toLower (Just 'L') ~?= Just 'l'
  ]

--------------------------------------------------------------------------------
-- 7
--------------------------------------------------------------------------------

tests7 = test $
  
  [
    -- leftDefault
    leftDefault ["s"] (Left True) ~?= ["s"]
  , leftDefault 1 (Right 2) ~?= 2
  , leftDefault "a" (Left 'a') ~?= ['a']
    -- rightDefault
  , rightDefault False (Left True) ~?= True
  , rightDefault 1 (Right 2) ~?= 1
  , rightDefault 'c' (Left 'a') ~?= 'a'
  ]



--------------------------------------------------------------------------------
-- 8
--------------------------------------------------------------------------------

tests8 = test $
  [ eitherBranch (+1) read (Right "10")                   ~?= 10
  , eitherBranch (+1) read (Left 100)                     ~?= 101
  , eitherBranch ("Hello " ++) ("Bye " ++) (Left "world") ~?= "Hello world"
  ]

--------------------------------------------------------------------------------
-- 9
--------------------------------------------------------------------------------

tests9 = test $
  [ plus (1+) show (Right 10)           ~?= (Right "10")
  , plus ord chr (Left 'Z')             ~?= (Left $ ord 'Z')
  , plus (map (+1)) sum (Right [1..10]) ~?= Right 55
  ]

--------------------------------------------------------------------------------
-- 10
--------------------------------------------------------------------------------

tests10 = test $
  [ prefix "" "Sed bibendum."  ~?= True
  , prefix "Hi" "Hi there"     ~?= True
  , prefix "Hi" "Hello, world" ~?= False
  , prefix "HE" "Hello, world" ~?= False
  , prefix "Hello, world" "H"  ~?= False
  , prefix
      (map chr [65..77])
      (map chr [65..90])       ~?= True
  , prefix
      "Sed bibendum. Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus."
      "Sed bibendum. Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit."
                               ~?= False
  ]

--------------------------------------------------------------------------------
-- 11
--------------------------------------------------------------------------------

tests11 = test $
  [ commonPrefix "Hello, everyone!" "Hello, world!" ~?= "Hello, "
  , commonPrefix "Star Wars" "Star Trek"            ~?= "Star "
  , commonPrefix "Radiohead" "The Decemberists"     ~?= ""
  , commonPrefix "" "Boop"                          ~?= ""
  , commonPrefix "Snoot" ""                         ~?= ""
  , commonPrefix "Zahnzusatzversicherung"
                 "Zahnzusatzversicherung"           ~?= "Zahnzusatzversicherung"
  ]

--------------------------------------------------------------------------------
-- 12
--------------------------------------------------------------------------------
unbranch :: Radix -> [(String, Radix)]
unbranch (Branch t) = t

r1, r2 :: Radix
r1 = Branch
       [("the "
         , Branch
             [("d", Branch
                      [ ("ecemberists.", Branch [])
                      , ("oors.", Branch [])])
              , ("mountain goats.", Branch []) ])]
r2 = Branch $ unbranch r1 ++ [("m", Branch [("gmt.", Branch []), ("odest mouse.", Branch [])])]


tests12 = test $
  [ strings empty     ~?= [""]
  , sort (strings t1) ~?= ["paper.", "plant."]
  , sort (strings t2) ~?= ["paper.", "plant.", "umbrella."]
  , sort (strings t3) ~?= ["test.", "tester.", "toast."]
  , sort (strings r1) ~?= ["the decemberists.", "the doors.", "the mountain goats."]
  , sort (strings r2) ~?= ["mgmt.", "modest mouse.", "the decemberists.", "the doors.", "the mountain goats."]
  ]

--------------------------------------------------------------------------------
-- 13
--------------------------------------------------------------------------------
norm :: Radix -> Radix
norm (Branch ts) = Branch . sortBy h2 . map h1 $ ts
  where
  h1 (p, r) = (p, norm r)
  h2 (p1, _) (p2, _) = compare p1 p2


tests13 = test $
  [ norm (insert "umbrella." t1) ~?= norm t2
  , sort (strings (insert "pleiades." t1)) ~?= ["paper.", "plant.", "pleiades."]
  , norm (insert "plant." (insert "paper." empty)) ~?= norm t1
  , norm (insert "paper." (insert "plant." empty)) ~?= norm t1
  , norm (insert "plant." t1) ~?= norm t1
  , sort (strings (insert "the white stripes." r1))
    ~?= ["the decemberists.", "the doors.", "the mountain goats.", "the white stripes."]
  , norm (insert "mgmt." (insert "modest mouse." r1))
    ~?= norm r2
  ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------
allTests :: [Test]
allTests =
  [ tests1 , tests2 , tests3  , tests4  , tests5  , tests6 , tests7 ,
    tests8 , tests9 , tests10 , tests11 , tests12 , tests13 ]

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
