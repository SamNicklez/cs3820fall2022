module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit


-- Lib
import Problems5 (
  Formula(..),
  nnf,
  isNnf,
  dnf,
  isDnf)


--------------------------------------------------------------------------------
-- -1 (infrastructure)
--------------------------------------------------------------------------------

vars (V s) = [s]
vars (C _) = []
vars (Not f) = vars f
vars (f :/\: g) = vars' f g
vars (f :\/: g) = vars' f g

vars' f g = vs ++ ws where
  ws = vars g
  vs = filter (`notElem` ws) (vars f)

eval h (V s)      = s `elem` h
eval _ (C b)      = b
eval h (Not f)      = not (eval h f)
eval h (f :/\: g) = eval h f && eval h g
eval h (f :\/: g) = eval h f || eval h g

same vs ws = length vs == length ws && all (`elem` ws) vs

equiv :: Formula -> Formula -> Bool
equiv f g = same vs (vars g) && and [eval h f == eval h g | h <- vss]
  where vs = vars f
        vss = map concat (sequence [[[], [v]] | v <- vs])

expectF :: Formula -> (Formula -> Formula) -> (Formula -> Bool) -> String -> String -> Test
expectF f p q s t =
  test [ assertBool (s ++ " (" ++ show f ++ ") is not in " ++ t) $
                     q (p f)
       , assertBool (s ++ " (" ++ show f ++ ") is not equivalent to " ++ show f) $
                      equiv (p f) f ]

expectNnf f = expectF f nnf isNnf "nnf" "negation normal form"
expectDnf f = expectF f dnf isDnf "dnf" "disjunctive normal form"
expectDnf' f = test (assertBool ("dnf (" ++ show f ++ ") is not in disjunctive normal form") (isDnf (dnf f)))

--------------------------------------------------------------------------------
-- 0 (definitions)
--------------------------------------------------------------------------------

vars1 = map (V . (:[])) ['a'..'z']
vars2 = map (V . (:[])) ['A'..'Z']

true  = C True
false = C False

--------------------------------------------------------------------------------
-- 1. Negation of Constants
--------------------------------------------------------------------------------

tests1 = test $ [
  nnf true        ~?= true,
  nnf false       ~?= false,
  nnf (Not false) ~?= true,
  nnf (Not true)  ~?= false
  ]

--------------------------------------------------------------------------------
-- 2. Double Negation
--------------------------------------------------------------------------------
tests2 = test $
         [expectNnf (Not (Not b)) | b <- [true, false]]
         ++
         [expectNnf (Not (Not (V [c]))) | c <- ['A'..'Z']]
         ++
         [let s = "a string with spaces and >1 characters."
          in expectNnf (Not (Not (V s)))]

--------------------------------------------------------------------------------
-- 3. Negation over ∧
--------------------------------------------------------------------------------
tests3 = test $
         [expectNnf (Not (C a :/\: C b)) | a <- [True, False], b <- [True, False]]
         ++
         [expectNnf (Not (V [a] :\/: V [b])) | a <- ['A'..'F'], b <- ['a'..'f']]

--------------------------------------------------------------------------------
-- 4. Negation over ∨
--------------------------------------------------------------------------------
tests4 = test $
         [expectNnf (Not (C a :\/: C b)) | a <- [True, False], b <- [True, False]]
         ++
         [expectNnf (Not (V [a] :\/: V [b])) | a <- ['A'..'F'], b <- ['a'..'f']]

--------------------------------------------------------------------------------
-- 5. Nested negations
--------------------------------------------------------------------------------
tests5 = test $
  [ expectNnf e1
  , expectNnf e2
  , expectNnf (Not . Not . Not . C $ True)
  , expectNnf (Not . Not . Not . C $ False)
  , expectNnf (Not . Not . C $ False)
  , expectNnf e3
  , expectNnf e4 ]
  where
  f n = foldl (.) id . replicate n $ Not
  e1 = f 10 (V "A")
  e2 = f 11 (V "A")
  e3 = f 3 (foldl (\ x y -> f 3 x :\/: f 2 y) (C False) (zipWith (\ x y -> f 2 x :\/: f 3 y) (take 7 vars1) vars2))
  e3' = (((C False :/\: (Not (V "a") :/\: V "A") :\/: (V "b" :\/: Not (V "B"))) :/\: (Not (V "c") :/\: V "C") :\/: (V "d" :\/: Not (V "D"))) :/\: (Not (V "e") :/\: V "E") :\/: (V "f" :\/: Not (V "F"))) :/\: (Not (V "g") :/\: V "G")
  e4 = f 3 (foldl (\ x y -> f 3 x :/\: f 2 y) (C False) (zipWith (\ x y -> f 2 x :/\: f 3 y) (take 7 vars1) vars2))

--------------------------------------------------------------------------------
-- 6. Distributing conjunction from the left
--------------------------------------------------------------------------------

tests6 = test $
  [ expectDnf e1
  , expectDnf' e2
  , expectDnf' e3
  , expectDnf' e4
  , expectDnf' e5
  ]
  where
  e1 = (vars1 !! 0) :/\: (vars1 !! 1 :\/: vars1 !! 2)
  e1' = ((vars1 !! 0) :/\: (vars1 !! 1)) :\/: ((vars1 !! 0) :/\: (vars1 !! 2))
  e2 = (V "ɸ") :/\: foldl (:\/:) (C False) vars1
  e2' = foldl (\ f c -> f :\/: (V "ɸ" :/\: c)) (V "ɸ" :/\: C False) vars1
  e3 = (V "ɸ") :/\: foldl (:\/:) (C False) (map Not vars1)
  e3' = foldl (\ f c -> f :\/: (V "ɸ" :/\: c)) (V "ɸ" :/\: C False) . map Not $ vars1
  e4 = (V "ɸ") :/\: (V "a" :/\: Not (V "b") :\/: V "b" :/\: Not (V "c"))
  e4' = (V "ɸ" :/\: (V "a" :/\: Not (V "b")) :\/: V "ɸ" :/\: (V "b" :/\: Not (V "c")))
  e5 = foldl (:/\:) (C True) vars1 :/\: (V "ɸ" :\/: V "ψ")
  e5' = foldl (:/\:) (C True) vars1 :/\: V "ɸ" :\/: foldl (:/\:) (C True) vars1 :/\: V "ψ"

--------------------------------------------------------------------------------
-- 7. Distributing conjunction from the right
--------------------------------------------------------------------------------

tests7 = test $
  [ expectDnf e1
  , expectDnf' e2
  , expectDnf' e3
  , expectDnf' e4
  , expectDnf' e5 ]
  where
  e1 = (vars1 !! 1 :\/: vars1 !! 2) :/\: (vars1 !! 0)
  e1' = ((vars1 !! 1) :/\: (vars1 !! 0)) :\/: ((vars1 !! 2) :/\: (vars1 !! 0))
  e2 =  foldl (:\/:) (C False) vars1 :/\: (V "ɸ")
  e2' = foldl (\ f c -> f :\/: (c :/\: V "ɸ")) (C False :/\: V "ɸ") vars1
  e3 = foldl (:\/:) (C False) (map Not vars1) :/\: (V "ɸ")
  e3' = foldl (\ f c -> f :\/: (c :/\: V "ɸ")) (C False :/\: V "ɸ") . map Not $ vars1
  e4 = (V "a" :/\: Not (V "b") :\/: V "b" :/\: Not (V "c")) :/\: (V "ɸ")
  e4' = (V "a" :/\: Not (V "b") :/\: V "ɸ" :\/: V "b" :/\: Not (V "c") :/\: V "ɸ")
  e5 = (V "ɸ" :\/: V "ψ") :/\: foldl (:/\:) (C True) vars1
  e5' = V "ɸ" :/\: foldl (:/\:) (C True) vars1 :\/: V "ψ" :/\: foldl (:/\:) (C True) vars1

--------------------------------------------------------------------------------
-- 8. Nested disjusnction
--------------------------------------------------------------------------------
tests8 = test $
  [ expectDnf (C True)
  , expectDnf (C False)
  , expectDnf (V "X")
  , expectDnf (Not (V "X"))
  , expectDnf e1
  , expectDnf e2
  , expectDnf' e3 ]
  where
  e1 = (C True :/\: (V "b" :\/: V "c")) :/\: (V "a" :/\: (C False :\/: V "d"))
  e1' = ((C True :/\: V "b") :/\: (V "a" :/\: C False) :\/: (C True :/\: V "b") :/\: (V "a" :/\: V "d"))
        :\/: ((C True :/\: V "c") :/\: (V "a" :/\: C False) :\/: (C True :/\: V "c") :/\: (V "a" :/\: V "d"))
  e2 = V "A" :/\: (V "B" :/\: V "C") :\/: V "A" :/\: (V "B" :/\: V "D")
  e3 = foldl (:/\:) (C True :\/: V "A") vars1
  e3' = foldl (:/\:) (C True) vars1 :\/: foldl (:/\:) (V "A") vars1

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------


allTests :: [Test]
allTests = [ tests1 , tests2 , tests3 , tests4 , tests5 , tests6
           , tests7 , tests8
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

