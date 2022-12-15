module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

import Data.Char (isSpace)

import Bird.Parser

-- Lib
import Problems13

assertElem :: (Eq a, Show a) => a -> [a] -> Test
assertElem a as = a `elem` as ~? "Expected " ++ show a ++ " to be in " ++ show as

--------------------------------------------------------------------------------
-- part 1
--------------------------------------------------------------------------------

checkBalanced :: String -> String -> Int -> Test
checkBalanced s leftover n = expected `assertElem` observed
  where
    expected = (replicate n 'a' ++ replicate n 'b', leftover)
    observed = (runParser balanced s)

p1 :: Test
p1 = test [
  checkBalanced "" "" 0,
  checkBalanced "aabb" "" 2,
  checkBalanced "abb" "b" 1,
  checkBalanced "aaaaabbbbbbbbbbbbb" "bbbbbbbb"  5,
  checkBalanced "aaaaaaaaabb" "aaaaaaaaabb" 0,
  checkBalanced "Wrong Characters" "Wrong Characters" 0,
  checkBalanced (replicate 100 'a' ++ replicate 101 'b') "b" 100
  ]

--------------------------------------------------------------------------------
-- part 2
--------------------------------------------------------------------------------

checkManyN :: Int -> Char -> String -> String -> Test
checkManyN n c s leftover =
   expected `assertElem` parsed
  where
    expected = (replicate n c, leftover)
    parsed = runParser (manyn n (satisfy (==c))) s

p2 :: Test
p2 = test [
          checkManyN 3 'a' "aaa" "",
          checkManyN 3 'b' "bbbbb" "bb",
          checkManyN 0 'a' "aaaa" "aaaa",
          checkManyN 7 'd' "ddddddd" "",
          ("foo", "BAR") `assertElem` runParser (manyn 3 lower) "fooBAR",
          ("F", "oobar") `assertElem` runParser (manyn 1 upper) "Foobar" ,
          ([1,2,3,4,5], "") `assertElem` runParser (manyn 5 digit) "12345"
          ]

p3 :: Test
p3 = test [
          ("abc", "") `assertElem` runParser balanced3 "abc",
          ("", "") `assertElem` runParser balanced3 "",
          ("aabbcc", "cc") `assertElem` runParser balanced3 "aabbcccc",
          let abc100 = (concatMap (replicate 100) ['a', 'b', 'c']) in
            (abc100, "") `assertElem` runParser balanced3 abc100,
          ("abc", "abcabc") `assertElem` runParser balanced3 "abcabcabc",
          runParser balanced3 "abbcc" ~?= [("", "abbcc")]
          ]

--------------------------------------------------------------------------------
-- part 3
--------------------------------------------------------------------------------

assertElemFloat :: (Double, String) -> [(Double, String)] -> Test
assertElemFloat p@(d, s) qs = go qs ~? "Expected " ++ show p ++ " to be in " ++ show qs
  where go []              = False
        go ((d', s') : qs) = (s == s' && abs (d - d') <= 0.001) || go qs

p4 :: Test
p4 = test [
  (4.3, "") `assertElemFloat` runParser float  "4.3" ,
  (-0.25, "") `assertElemFloat` runParser float  "-.25" ,
  (4.0, ".") `assertElemFloat` runParser float  "4." ,
  (1337.1337, "") `assertElemFloat` runParser float  "1337.1337" ,
  (-1, "") `assertElemFloat` runParser float  "-1",
  (0.0, "") `assertElemFloat` runParser float  "00000.000000",
  (0.01, "") `assertElemFloat` runParser float  ".01",
  (55.0, "") `assertElemFloat` runParser float  "55",
  runParser float "-." ~?= [],
  runParser float  "." ~?= []
  ]


argMap :: Int -> Test
argMap 1 = p1
argMap 2 = p2
argMap 3 = p3
argMap 4 = p4
argMap _ = test [p1, p2, p3, p4]

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
