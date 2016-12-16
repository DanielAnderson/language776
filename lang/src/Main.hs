{-# LANGUAGE GADTs, KindSignatures, InstanceSigs, OverloadedStrings  #-}

module Main where

import Lang
import Data.List

data TestAST:: * where
    TestEval   :: String -> AST -> Value -> TestAST
    TestTypeCheck :: String -> AST -> Maybe Type -> TestAST

data TestResult:: * where
    TestRunResult :: String -> Bool -> AST -> Value -> Value -> TestResult -- Discription, pass/fail, input, expected, actual
    TestTypeResult :: String -> Bool -> AST -> Maybe Type -> Maybe Type -> TestResult
instance Show TestAST where
    show (TestEval disc _ _) = disc
    show (TestTypeCheck disc _ _) = disc

showActualExpected actual expected = 
    "\n\tActual: " ++ (show actual)
    ++ "\n\tExpected: " ++ (show expected)

instance Show TestResult where
    show (TestRunResult disc True _ _ _) = "Passed: " ++ disc
    show (TestRunResult disc False input expected actual) = 
        "Failed: " ++ disc ++ showActualExpected actual expected
    show (TestTypeResult disc True _ _ _ ) = "Passed: " ++ disc
    show (TestTypeResult disc False input expected actual) =
        "Failed: " ++ disc ++ showActualExpected actual expected


getDisc::TestAST -> String
getDisc (TestEval disc _ _ ) = disc
getDisc (TestTypeCheck disc _ _) = disc

main :: IO ()
main = putStrLn $ printTests tests


testInt = TestEval "Testing integer" 1 (IntV 1)
testAdd = TestEval "Testing Addition" (1 + 2) (IntV 3)
testSub = TestEval "Testing Subtraction" (1 - 2) (IntV (-1))
testMult = TestEval "Testing Multiplication" (1 * 2) (IntV 2)
testBoolTrue = TestEval "Testing boolean true" (BoolT True) (BoolV True)
testBoolFalse = TestEval "Testing boolean false" (BoolT False) (BoolV False)
testIfTrue = TestEval "Testing true condition" (If (BoolT True) 1 2) (IntV 1)
testIfFalse = TestEval "Testing false condition" (If (BoolT False) 1 2) (IntV 2)
testUndefined = TestEval "Testing undefined var" (VarT "foo") (ErrorV "Undefined variable: foo")
testDefined = TestEval "Testing defined var" (Let "x" 2 (Add (VarT "x") 3)) (IntV 5)
testLambda = TestEval "Testing lambda" (LamT "x" IntType (Add (VarT "x") 3)) (ClosureV "x" (Add (VarT "x") 3) [])
testApp = TestEval "Testing application" (App (LamT "x" IntType (Add (VarT "x") 3)) 4) (IntV 7)
testIntType = TestTypeCheck "Testing typecheck: Integer" 
                            1 
                            $ return IntType
testBoolType = TestTypeCheck "Testing typecheck: Boolean"
                            (BoolT True) 
                            $ return BoolType
testLamType = TestTypeCheck "Testing typecheck: Int->Int"
                            (LamT "x" IntType (Add (VarT "x") 3))
                            $ return (LamType IntType IntType) 
testFailType = TestTypeCheck "Testing failure of typecheck with bool + int"
                            (LamT "x" BoolType (Add (VarT "x") 1))
                            Nothing
testIfType = TestTypeCheck "Testing simple if statement"
                            (If (BoolT True) 1 2)
                            $ return IntType
testIfType' = TestTypeCheck "Testing more complicated if statement"
                            (If 
                                (App 
                                    (LamT "x" BoolType (If (VarT "x") (BoolT False) (BoolT True)))
                                    (BoolT False)
                                )
                                1
                                2)
                            $ return IntType
testAppType = TestTypeCheck "Testing application"
                            (App
                                (LamT "x" BoolType (If (VarT "x") 1 2))
                                (BoolT True)
                                )
                            $ return IntType
testLetType = TestTypeCheck "Testing let"
                            (Let "x" 2 (Add (VarT "x") (VarT "x")))
                            $ return IntType
testGE = TestEval "Testing GE"
                    (CompInt Ge 2 1)
                    (BoolV True)
testGE' = TestEval "Testing GE"
                    (CompInt Ge 1 2)
                    (BoolV False)
testGE'' = TestEval "Testing GE"
                    (CompInt Ge 1 1)
                    (BoolV True)
testLE = TestEval "Testing LE"
                    (CompInt Le 2 1)
                    (BoolV False)
testLE' = TestEval "Testing LE"
                    (CompInt Le 1 2)
                    (BoolV True)
testLE'' = TestEval "Testing LE"
                    (CompInt Le 1 1)
                    (BoolV True)
testGT = TestEval "Testing Gt"
                    (CompInt Gt 2 1)
                    (BoolV True)
testGT' = TestEval "Testing Gt"
                    (CompInt Gt 1 2)
                    (BoolV False)
testGT'' = TestEval "Testing Gt"
                    (CompInt Gt 1 1)
                    (BoolV False)
testLT = TestEval "Testing LT"
                    (CompInt Lt 2 1)
                    (BoolV False)
testLT' = TestEval "Testing LT"
                    (CompInt Lt 1 2)
                    (BoolV True)
testLT'' = TestEval "Testing LT"
                    (CompInt Lt 1 1)
                    (BoolV False)
testEq = TestEval "Testing EQ"
                    (CompInt Eq 1 2)
                    (BoolV False)
testEq' = TestEval "Testing EQ"
                    (CompInt Eq 1 1)
                    (BoolV True)     
testNE = TestEval "Testing NE"
                    (CompInt Ne 1 2)
                    (BoolV True)
testNE' = TestEval "Testing NE"
                    (CompInt Ne 1 1)
                    (BoolV False)     
testDividesConst = TestEval "Testing function to determine if the first argument is divisble by the two"
                        (App (LamT "num" (LamType IntType BoolType) (CompInt Eq 0 (Mod (VarT "num") 2))) 4) 
                        (BoolV True)
testFact = TestEval "Testing recursive function for factoral" 
            (Let 
                "fact"
                (LamT "x" 
                      (LamType IntType IntType) 
                      (If 
                        (CompInt Eq (VarT "x") 0) 
                        1 
                        (Mult (VarT "x") (App (VarT "fact") (Sub (VarT "x") 1)))
                        )
                      )
                (App (VarT "fact") 15)
                )
            (IntV (product [1..15]))

fib:: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

testFib = TestEval "Testing fibonacci function"
            (Let
                "fib"
                (LamT 
                    "x"
                    (LamType IntType IntType)
                    (If
                        (CompInt Eq (VarT "x") 0)
                        1
                        (If 
                            (CompInt Eq (VarT "x") 1)
                            1
                            (Add
                                (App (VarT "fib") (Sub (VarT "x") 1))
                                (App (VarT "fib") (Sub (VarT "x") 2))
                                )
                            )
                        )
                    )
                (App (VarT "fib") 5)
                )
            (IntV (fib 5))

dividesAST::AST
dividesAST = (LamT
                    "x"
                    (LamType IntType (LamType IntType BoolType))
                    (LamT
                        "y"
                        (LamType IntType BoolType)
                        (CompInt Eq (Mod (VarT "x") (VarT "y")) 0)
                        )
                    )

testDivides :: Int -> Int -> TestAST
testDivides x y = TestEval ("Testing function that takes two arguments and returns if the first is divisible by the second on: " ++ show x ++ ", " ++ show y)
            (Let
                "divides"
                dividesAST
                (App (App (VarT "divides") (IntT x)) (IntT y)))
                (BoolV ((mod x y) == 0))

isPrimeRecAST::AST
isPrimeRecAST = (LamT
                    "x"
                    (LamType IntType (LamType IntType BoolType))
                    (LamT 
                        "y"
                        (LamType IntType BoolType)
                        (If (CompInt Eq (VarT "y") 1)
                            (BoolT True)
                            (If (App (App (VarT "divides") (VarT "x")) (VarT "y"))
                                (BoolT False)
                                (App (App (VarT "isPrimeRec") (VarT "x")) (Sub (VarT "y") 1))
                                ) 
                            )
                        )
                    )

isPrimeAST::AST
isPrimeAST =(LamT 
                "candidate"
                (LamType IntType BoolType)
                (App 
                    (App 
                        (VarT "isPrimeRec") 
                        (VarT "candidate")
                        )
                    (Sub (VarT "candidate") 1)) -- Note, this can obviously be made faster starting with square root of the candidate
                )

isPrime::Int -> Bool
isPrime x = (x > 1) && (and (map (\y -> (mod x y) /= 0) [2..x-1]))

testPrimality :: Int -> TestAST
testPrimality x = TestEval ("Testing function to determine primality of: " ++ show x)
            (Let
                "divides"
                dividesAST
                (Let
                    "isPrimeRec"
                    isPrimeRecAST
                    (Let "isPrime"
                        isPrimeAST
                        (App (VarT "isPrime") (IntT x))
                    )
                )
                )
            (BoolV (isPrime x))

tests = [testInt, testAdd, testSub, testMult, testBoolTrue, 
        testBoolFalse, testIfTrue, testIfFalse, testUndefined,
        testDefined, testLambda, testApp, testIntType, testBoolType,
        testLamType, testFailType, testIfType, testIfType',
        testAppType, testLetType, testGE,testGE',testGE'',
        testLE,testLE',testLE'',testGT,testGT',testGT'',
        testLT,testLT',testLT'',testEq,testEq',testNE,testNE', 
        testDividesConst, testFact, testFib, testDivides 18 3,
        testDivides 18 5, testDivides 19 19, testPrimality 15, testPrimality 2,
        testPrimality 3, testPrimality 32, testPrimality 29]

runTest::TestAST -> TestResult
runTest (TestEval disc input expected) =
    TestRunResult disc (actual == expected) input expected actual
    where actual = (eval input)

runTest (TestTypeCheck disc input expected) = 
    TestTypeResult disc (actual == expected ) input expected actual
    where actual = (typeCheck [] input)

printTests tests = intercalate "\n" (map show ranTests) ++ "\nPassed: " ++ show (numPassed ranTests) ++ "/" ++ show (length tests) ++ " tests"
    where ranTests = runTests tests

runTests tests = map runTest tests
passed (TestRunResult _ didPass _ _ _  ) = didPass
passed (TestTypeResult _ didPass _ _ _ ) = didPass
numPassed tests = length $ filter passed tests