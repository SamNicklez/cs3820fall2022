{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit

-- Lib
import Problems10

finite :: Var ts t -> Bool
finite VZ = True
finite (VS z) = finite z

noConst :: Expr ts t -> Bool
noConst (Const _) = False
noConst (Var z) = finite z
noConst (Lam m) = noConst m
noConst (App m n) = noConst m && noConst n
noConst (Pair m n) = noConst m && noConst n
noConst (PrjL m) = noConst m
noConst (PrjR m) = noConst m
noConst (InjL m) = noConst m
noConst (InjR m) = noConst m
noConst (Case l m n) = and [noConst l, noConst m, noConst n]

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

swaparoonie :: Expr '[] ((t, u) -> (u, t))
swaparoonie = swapE

leaningRight :: Expr '[] (((t, u), v) -> (t, (u, v)))
leaningRight = assocRightE

rightRoundBabyRightRound :: Expr '[] (Either t u -> Either u t)
rightRoundBabyRightRound = rotateE

t1, t2, t3 :: Test

t1 = test [noConst swaparoonie @? ""]

t2 = test [noConst leaningRight @? ""]

t3 = test [noConst rightRoundBabyRightRound @? ""]

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


t4, t5, t6 :: Test

t4 = test [ eval (Pair (Const 1) (Const 1)) ENil @?= (1, 1)
          , eval (Pair (Var VZ) (Pair (Var (VS VZ)) (Const 3))) bigEnv @?= (True, ("foo", 3))]
t5 = test [ eval (PrjL (Const (1, 2))) ENil @?= 1
          , eval (PrjL (Pair (Var VZ) (Pair (Var (VS VZ)) (Const 3)))) bigEnv @?= True ]
t6 = test [ eval (PrjR (Const (1, 2))) ENil @?= 2
          , eval (PrjR (Pair (Var VZ) (Pair (Var (VS VZ)) (Const 3)))) bigEnv @?= ("foo", 3) ]

t7, t8,  t9 :: Test

t7 = test [ eval (InjL (Const 1)) ENil @?= (Left 1 :: Either Int Bool)
          , eval (InjL (InjL (Const True))) ENil @?= (Left (Left True) :: Either (Either Bool Int) Bool)
          , eval (InjL (Var (VS (VS (VS VZ))))) bigEnv @?= (Left 2.0 :: Either Float Bool) ]

t8 = test [ eval (InjR (Const 1)) ENil @?= (Right 1 :: Either Bool Int)
          , eval (InjR (InjR (Const 2.0))) ENil @?= (Right (Right 2.0) :: Either Bool (Either Bool Float))
          , eval (InjR (Var (VS (VS (VS (VS (VS VZ))))))) bigEnv @?= (Right [1, 2, 3] :: Either Bool [Int]) ]
t9 = test [ eval (Case (Const (Left 1)) (Var VZ) (Var VZ)) ENil @?= (1 :: Int)
          , eval (Case (Const (Right True)) (Const True) (App (Const not) (Var VZ))) ENil @?= False
          , eval (Case (InjL (InjR (Var (VS (VS VZ)))))
                       (Case (Var VZ)
                             (Const 2)
                             (App (Const negate) (Var VZ)))
                       (Const 3)) bigEnv @?= -1 ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests = [t1, t2, t3, t4, t5, t6, t7, t8, t9]

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

        testsJson = unlines [ "{\"name\" : \"Problem set 10, problem " ++ show n ++ "\", \"setup\" : \"\", \"run\" : \"cabal run Test " ++ show n ++ "\", \"input\" : \"\", \"output\" : \"\", \"comparison\" : \"included\", \"timeout\" : 0.5, \"points\" : 1 },"
                            | (n, _) <- zip [1..] allTests
                            ]
