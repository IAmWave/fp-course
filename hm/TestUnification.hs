module TestUnification where

import Test.HUnit

import Unification
import Types

testMguWorks :: MonoT -> MonoT -> Bool
testMguWorks t1 t2 = t1' == t2'
    where
        u = mgu t1 t2
        PolyTMono t1' = u (PolyTMono t1)
        PolyTMono t2' = u (PolyTMono t2)

test1 :: Test
test1 = TestCase $ assertEqual
    "Consts"
    (testMguWorks (TConst 1) (TConst 1))
    True

test2 :: Test
test2 = TestCase $ assertEqual
    "Functions on consts"
    (testMguWorks f (Function (TConst 1) (TConst 2)))
    True
    where f = (Function (TConst 1) (TConst 2))

test3 :: Test
test3 = TestCase $ assertEqual
    "Var to const"
    (testMguWorks (TConst 1) (TVar 2))
    True

test4 :: Test
test4 = TestCase $ assertEqual
    "Functions with vars"
    (testMguWorks
        (Function (TVar 1) (TVar 2))
        (Function (TVar 3) (TVar 4)))
    True

test5 :: Test
test5 = TestCase $ assertEqual
    "Functions with vars + consts"
    (testMguWorks
        (Function (TVar 1) (TConst 2))
        (Function (TConst 3) (TVar 4)))
    True

test6 :: Test
test6 = TestCase $ assertEqual
    "Higher order functions"
    (testMguWorks
        (Function (TVar 1) (TVar 1))
        (Function (Function (TVar 2) (TVar 2)) (TVar 3)))
    True

runUnificationTests :: IO Counts
runUnificationTests = runTestTT $ TestList [
        test1, test2, test3, test4, test5, test6
    ]

main :: IO Counts
main = runUnificationTests

