module Tests where

-- GHC
import System.Exit
import System.Environment
import System.IO.Silently

-- External
import Test.HUnit

import Data.Char (isSpace)
import Control.Monad
import Control.Monad.State

-- Lib
import Problems12

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

captureCount :: String -> Int -> String
captureCount = evalState . addCount

p1 :: Test
p1 = test [
  captureCount "fo ob ar" 0
    @?= "fo ob ar" ++ (replicate 69 ' ') ++ "(3)",
  captureCount "1" 0
    @?= "1" ++ (replicate 76 ' ') ++ "(1)",
  captureCount "1" 80
    @?= "1" ++ (replicate 75 ' ') ++ "(81)",
  captureCount "fo ob ar" 4
    @?= "fo ob ar" ++ (replicate 69 ' ') ++ "(7)",
  let s = (concat (replicate 15 "five ")) in
    captureCount s 0 @?= s ++ " (15)",
  captureCount
  ("The smallest positive integer not definable in under sixty letters") 0
    @?= "The smallest positive integer not definable in under sixty letters" ++ (replicate 10 ' ')
    ++ "(10)"
  ]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

captureCounts :: [String] -> Int -> [String]
captureCounts = evalState . addCounts

p2 :: Test
p2 = test [
  captureCounts [] 0 @?= [],
  captureCounts [replicate 77 '0'] 0 @?= [replicate 77 '0' ++ "(1)"],
  captureCounts ["fo ob ar", "or", "not", "to", "fo ob ar"] 0
    @?= [
      "fo ob ar" ++ (replicate 69 ' ') ++ "(3)",
      "or" ++ (replicate 75 ' ') ++ "(4)",
      "not" ++ (replicate 74 ' ') ++ "(5)",
      "to" ++ (replicate 75 ' ') ++ "(6)",
      "fo ob ar" ++ (replicate 69 ' ') ++ "(9)"
      ],
    captureCounts ["I like a look of agony,", "Because I know it's true", "Men do not sham Convulsion,"] 100
      @?= [
      "I like a look of agony," ++ replicate 52 ' ' ++ "(106)",
      "Because I know it's true" ++ replicate 51 ' ' ++ "(111)",
      "Men do not sham Convulsion," ++ replicate 48 ' ' ++ "(116)"
      ]

  ]

--------------------------------------------------------------------------------
-- Part 3
--------------------------------------------------------------------------------
sample1   = "foobar"
expected1 = "foobar" ++ (replicate 71 ' ') ++ "(1)\n"


sample2 = "What a waste it is to lose one's mind. Or not\n\
          \to have a mind is being very wasteful.\n\
          \How true that is."

expected2 =
  "What a waste it is to lose one's mind. Or not                               (11)\n\
  \to have a mind is being very wasteful.                                      (19)\n\
  \How true that is.                                                           (23)\n"

sample3 =
  "Hawaii has always been a very pivotal role in the Pacific.\n\
  \It is in the Pacific.\n\
  \It is a part of the United States that is an island\n\
  \that is right there."

expected3 =
  "Hawaii has always been a very pivotal role in the Pacific.                  (11)\n\
  \It is in the Pacific.                                                       (16)\n\
  \It is a part of the United States that is an island                         (28)\n\
  \that is right there.                                                        (32)\n"

sample4 =
  "This\n\
   \\n\n\n\n\n\n\
  \has too many\n\
  \blank\n\
  \lines"

expected4 =
  "This                                                                         (1)\n\
  \\n\n\n\n\n\n\
  \has too many                                                                 (4)\n\
  \blank                                                                        (5)\n\
  \lines                                                                        (6)\n"


testIO :: String -> String -> IO Test
testIO s expected = do
  (actual, _) <- capture . printLines $ s
  return . test $ (actual @?= expected)

p3 :: IO [Test]
p3 = sequence [
  testIO sample1 expected1,
  testIO sample2 expected2,
  testIO sample3 expected3,
  testIO sample4 expected4
  ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

argMap :: Int -> IO Test
argMap 1 = return p1
argMap 2 = return p2
argMap 3 = do
  tests <- p3
  return . test $ tests
argMap _ = do
  p3' <- p3
  return . test $ [p1, p2, test p3']

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  tests <- case read <$> (hd args) of
                Just x -> argMap x
                Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
