module TestHM where

import Test.HUnit

import HM
import Types

gt :: TContext
gt= [
        (Var 1, PolyTMono $ TConst 1),
        (Var 2, PolyTMono $ TConst 2),
        (Var 3, PolyTMono $ TVar 1)
    ]

inferPolyT :: Term -> PolyT
inferPolyT tr = t
    where
        (_, t, _) = w gt tr 100

inferMonoT :: Term -> MonoT
inferMonoT tr = m
    where PolyTMono m = inferPolyT tr

test1 :: Test
test1 = TestCase $ assertEqual
    "Single variable"
    (inferMonoT (Var 1))
    (TConst 1)

test2 :: Test
test2 = TestCase $ assertEqual
    "Abstraction"
    (inferMonoT (Abs 3 (Var 1)))
    (Function (TVar 100) (TConst 1))

test3 :: Test
test3 = TestCase $ assertEqual
    "Application - const"
    (inferPolyT (App (Abs 5 (Var 1)) (Var 2)))
    vt
    where
        Just vt = (inferType gt (Var 1))

test4 :: Test
test4 = TestCase $ assertEqual
    "Application - id"
    (inferPolyT (App (Abs 5 (Var 5)) (Var 2)))
    vt
    where
        Just vt = (inferType gt (Var 2))

test5 :: Test
test5 = TestCase $ assertEqual
    "Generalize"
    (generalize gt (PolyTMono $ Function (TVar 1) (TVar 5)))
    (PolyTAbs 5 $ PolyTMono $ Function (TVar 1) (TVar 5))
    where
        Just vt = (inferType gt (Var 2))

test6 :: Test
test6 = TestCase $ assertEqual
    "Let 1"
    (inferPolyT e)
    (inferPolyT (Var 2))
    where
        e = Let 5 (Var 2) (Var 5)

test7 :: Test
test7 = TestCase $ assertEqual
    "Let function"
    (inferPolyT (Abs 3 (Var 2)))
    (inferPolyT e)
    where
        e = Let 5 (Var 2) (Abs 3 (Var 5))

test8 :: Test
test8 = TestCase $ assertEqual
    "Let - polymorphic identity example"
    a b
    where
        e = Let 5 (Abs 6 (Var 6)) (App (Var 5) (Var 5))
        (Function a b) = inferMonoT e

test9 :: Test
test9 = TestCase $ assertEqual
    "Rename quantifiers"
    (f t)
    t2
    where
        t = (PolyTAbs 2 (PolyTMono $ Function (TVar 2) (TVar 2)))
        t2 = (PolyTAbs vi (PolyTMono $ Function (TVar vi) (TVar vi)))
        (f, _) = renameQuantifiers t vi
        vi = 100


runHMTests :: IO Counts
runHMTests = runTestTT $ TestList [
        test1, test2, test3, test4, test5, test6, test7, test8, test9
    ]

main :: IO Counts
main = runHMTests
