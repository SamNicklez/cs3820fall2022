{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems9

finite :: Var ts t -> Bool
finite VZ = True
finite (VS z) = finite z

noConst :: Expr ts t -> Bool
noConst (Const _) = False
noConst (Var z) = finite z
noConst (Lam m) = noConst m
noConst (App m n) = noConst m && noConst n

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

secondE :: Expr '[] (t -> u -> u)
secondE = tsnocE

flipadelphia :: Expr '[] ((t -> u -> v) -> u -> t -> v)
flipadelphia = flipE

t1, t2 :: Test

t1 = test [noConst secondE @? ""]

t2 = test [noConst flipadelphia @? ""]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

bigEnv :: Env '[Bool, String, Int, Float, (a -> a), [Int]]
bigEnv =
  ECons True      $
  ECons "foo"     $
  ECons 1         $
  ECons 2.0       $
  ECons id        $
  ECons [1, 2, 3] $
  ENil


t3, t4 :: Test

t3 = test
  [ var VZ (ECons 1 ENil) @?= 1
  , var VZ (ECons "a" ENil) @?= "a"
  , var VZ (ECons 1.0 ENil) @?= 1.0
  , var VZ bigEnv           @?= True
  ]

t4 = test
  [ var (VS VZ) bigEnv      @?= "foo"
  , var (VS $ VS VZ) bigEnv @?= 1
  , var (VS $ VS $ VS VZ) bigEnv @?= 2.0
  , (var (VS $ VS $ VS $ VS VZ) bigEnv) "id" @?= "id"
  , (var (VS $ VS $ VS $ VS $ VS VZ) bigEnv) @?= [1, 2, 3]
  ]

t5, t6, t7, t8 :: Test

t5 = test
  [ eval (Const 1) bigEnv @?= 1
  , eval (Const "foo") bigEnv @?= "foo"
  , eval (Const 'a')   bigEnv @?= 'a'
  ]

t6 = test
  [ eval (Var VZ) (ECons "a" ENil) @?= "a"
  , eval (Var $ VS $ VS $ VS $ VS $ VS VZ) bigEnv @?= [1, 2, 3]
  ]

t7 = test
  [ eval idE ENil 1 @?= 1
  , eval constE ENil 'a' 2 @?= 'a'
  , eval secondE ENil 'a' 2 @?= 2
  ]

t8 = test
  [ eval ppaE ENil 1 (+2) @?= 3
  , eval ppaE ENil "foo" (++ "bar") @?= "foobar"
  , eval sE   ENil (+) (*2) 1 @?= 3
  , eval (App flipE secondE) ENil 'a' 2 @?= 'a'
  ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests = [t1, t2, t3, t4, t5, t6, t7, t8]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("json" : _) ->
        do putStrLn testsJson
           exitWith ExitSuccess
    [] -> runTests allTests
    [n] -> runTests [allTests !! (read n - 1)]
    [m, n] -> runTests (take (read n - read m) (drop (read m - 1) allTests))

  where runTests tests =
            do results <- runTestTT (test tests)
               if errors results + failures results == 0
               then exitWith ExitSuccess
               else exitWith (ExitFailure 1)

        testsJson = unlines [ "{\"name\" : \"Problem set 9, problem " ++ show n ++ "\", \"setup\" : \"\", \"run\" : \"cabal run Test " ++ show n ++ "\", \"input\" : \"\", \"output\" : \"\", \"comparison\" : \"included\", \"timeout\" : 0.5, \"points\" : 1 },"
                            | (n, _) <- zip [1..] allTests
                            ]
