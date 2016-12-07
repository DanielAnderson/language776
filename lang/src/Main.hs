{-# LANGUAGE GADTs, KindSignatures, InstanceSigs, OverloadedStrings  #-}

module Main where

import Lang
import Data.List

data TestAST:: * where
    TestEval   :: String -> AST -> Value -> TestAST
    TestTypeCheck :: String -> AST -> (Maybe Type) -> TestAST

data TestResult:: * where
    TestResult :: String -> Bool -> TestResult 

instance Show TestAST where
    show (TestEval disc _ _) = disc
    show (TestTypeCheck disc _ _) = disc

instance Show TestResult where
    show (TestResult disc passed) = (if passed then "Passed: " else "Failed: ") ++ disc

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
testUndefined = TestEval "Testing undefined var" (VarT "foo") (ErrorV "No definition of variable: foo")
testDefined = TestEval "Testing defined var" (Let "x" 2 (Add (VarT "x") 3)) (IntV 5)
testLambda = TestEval "Testing lambda" (LamT (VarT "x") IntType (Add (VarT "x") 3)) (ClosureV "x" (Add (VarT "x") 3) [])
testApp = TestEval "Testing application" (App (LamT (VarT "x") IntType (Add (VarT "x") 3)) 4) (IntV 7)
testIntType = TestTypeCheck "Testing typecheck: Integer" 
                            1 
                            $ return IntType
testBoolType = TestTypeCheck "Testing typecheck: Boolean"
                            (BoolT True) 
                            $ return BoolType
testLamType = TestTypeCheck "Testing typecheck: Int->Int"
                            (LamT (VarT "x") IntType (Add (VarT "x") 3))
                            $ return (LamType IntType IntType) 
testFailType = TestTypeCheck "Testing failure of typecheck with bool + int"
                            (LamT (VarT "x") BoolType (Add (VarT "x") 1))
                            Nothing
testIfType = TestTypeCheck "Testing simple if statement"
                            (If (BoolT True) 1 2)
                            $ return IntType
testIfType' = TestTypeCheck "Testing more complicated if statement"
                            (If 
                                (App 
                                    (LamT (VarT "x") BoolType (If (VarT "x") (BoolT False) (BoolT True)))
                                    (BoolT False)
                                )
                                1
                                2)
                            $ return IntType
testAppType = TestTypeCheck "Testing application"
                            (App
                                (LamT (VarT "x") BoolType (If (VarT "x") 1 2))
                                (BoolT True)
                                )
                            $ return IntType
testLetType = TestTypeCheck "Testing let"
                            (Let "x" 2 (Add (VarT "x") (VarT "x")))
                            $ return IntType
tests = [testInt, testAdd, testSub, testMult, testBoolTrue, 
        testBoolFalse, testIfTrue, testIfFalse, testUndefined,
        testDefined, testLambda, testApp, testIntType, testBoolType,
        testLamType, testFailType, testIfType, testIfType',
        testAppType, testLetType]

runTest::TestAST -> TestResult
runTest (TestEval disc input correctOutput) =
    TestResult disc ((runLang [] input) == correctOutput)
runTest (TestTypeCheck disc input correctType) = 
    TestResult disc ((typeCheck [] input) == correctType )

printTests tests = intercalate "\n" (map show ranTests) ++ "\nPassed: " ++ show (numPassed ranTests) ++ "/" ++ show (length tests) ++ " tests"
    where ranTests = runTests tests

runTests tests = map runTest tests
passed (TestResult _ didPass) = didPass
numPassed tests = length $ filter passed tests