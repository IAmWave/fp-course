module TestLambda where

import Data.Maybe
import Test.HUnit
import Lambda

kComb = Abstraction (Abstraction (Var 2))
iComb = Abstraction (Var 1)
omegaComb = Abstraction (Application (Var 1) (Var 1)) -- ω combinator
omegaAppl = Application omegaComb omegaComb


testK :: Test
testK = TestCase $ assertEqual
    "K 123 -> λ 124"
    (reduce testInput) testOutput
    where
        testInput = Application kComb (Var 123)
        testOutput = Abstraction (Var 124)


testKI :: Test
testKI = TestCase $ assertEqual
    "KI -> λI"
    (reduce testInput) testOutput
    where
        testInput = Application kComb iComb
        testOutput = Abstraction iComb


testOmega :: Test
testOmega = TestCase $ assertEqual
    "ωω -> ωω"
    (fromMaybe (Var (-1)) (step omegaAppl)) omegaAppl


testKIOmega :: Test
testKIOmega = TestCase $ assertEqual
    "KIω -> I"
    (reduce testInput) testOutput
    where
        testInput = Application (Application kComb iComb) omegaAppl
        testOutput = iComb


-- Example from https://en.wikipedia.org/wiki/De_Bruijn_index
testWiki :: Test
testWiki = TestCase $ assertEqual
    "Wikipedia example: (λ λ 4 2 (λ 1 3)) (λ 5 1) -> λ 3 (λ 6 1) (λ 1 (λ 7 1))"
    (reduce testInput) testOutput
    where 
        testInput =
            Application
                (Abstraction
                    (Abstraction
                        (Application
                            (Application
                                (Var 4)
                                (Var 2))
                            (Abstraction (
                                Application
                                (Var 1)
                                (Var 3))))))
                (Abstraction (Application (Var 5) (Var 1)))
        testOutput =
            Abstraction $
                Application
                    (Application
                        (Var 3)
                        (Abstraction $
                            Application
                                (Var 6)
                                (Var 1)))
                    (Abstraction $
                        Application
                            (Var 1)
                            (Abstraction $
                                Application
                                    (Var 7)
                                    (Var 1)))


testShowApply1 :: Test
testShowApply1 = TestCase $ assertEqual
    "(1 2) 3 displays without parentheses"
    (show testInput) "1 2 3"
    where testInput = Application (Application (Var 1) (Var 2)) (Var 3)


testShowApply2 :: Test
testShowApply2 = TestCase $ assertEqual
    "1 (2 3) displays with parentheses"
    (show testInput) "1 (2 3)"
    where testInput = Application (Var 1) (Application (Var 2) (Var 3))


main :: IO Counts
main = runTestTT $ TestList [testK, testKI, testOmega, testWiki, testShowApply1, testShowApply2]
