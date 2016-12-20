-- Written by: Daniel Anderson
-- EECS 776 Practical
-- Citations:
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling#the-either-monad
-- Lang.hs
{-# LANGUAGE GADTs, KindSignatures, InstanceSigs, OverloadedStrings  #-}
module Lang where
    
import GHC.Exts()
import Control.Monad (ap)

toIntCompare:: IntComparison -> (Int -> Int -> Bool)
toIntCompare Ge = (>=)
toIntCompare Le = (<=)
toIntCompare Gt = (>)
toIntCompare Lt = (<)
toIntCompare Ne = (/=)
toIntCompare Eq = (==)

data BoolOperation = And | Or deriving (Show, Eq)

toBoolOp::BoolOperation -> (Bool -> Bool -> Bool)
toBoolOp And = (&&)
toBoolOp Or = (||)

data AST :: * where
    BoolT    :: Bool -> AST
    IntT     :: Int  -> AST
    VarT     :: String -> AST
    LamT     :: String -> Type -> AST -> AST           --VarT, Body
    If       :: AST -> AST -> AST -> AST
    Add      :: AST -> AST -> AST
    Mult     :: AST -> AST -> AST
    Sub      :: AST -> AST -> AST
    Div      :: AST -> AST -> AST
    Mod      :: AST -> AST -> AST
    Let      :: String -> AST -> AST -> AST
    App      :: AST -> AST -> AST
    CompInt  :: IntComparison -> AST -> AST -> AST
    BoolOp2  :: BoolOperation -> AST -> AST -> AST 
    deriving (Show, Eq)



instance Num AST where
    fromInteger :: Integer -> AST
    fromInteger = IntT . fromInteger

    (+) :: AST -> AST -> AST
    (+) = Add

    (-) :: AST -> AST -> AST
    (-) = Sub

    (*) :: AST -> AST -> AST
    (*) = Mult

type Env = [(String, Value)]
type Context = [(String, Type)]
newtype Evaluator a = Ev (Either Value a)
newtype TypeChecker a = Ty (Either String a) deriving (Show, Eq)
data IntComparison = Gt | Lt | Ge | Le | Eq | Ne deriving (Show, Eq)

data Value:: * where
    BoolV    :: Bool -> Value
    IntV     :: Int -> Value
    ClosureV :: String -> AST -> Env -> Value
    ErrorV   :: String -> Value
    deriving (Show, Eq)


data Type:: * where
    IntType     :: Type
    BoolType    :: Type
    LamType     :: Type -> Type -> Type -- Argtype, return type
    FailureType :: String -> Type
    deriving (Show, Eq)

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg)
          Right v -> k v
    return v = Ev (Right v)
    fail msg = Ev (Left (ErrorV msg))

instance Monad TypeChecker where
    (Ty ev) >>= k = 
        case ev of
            Left msg -> Ty (Left msg)
            Right v -> k v
    return v = Ty (Right v)
    fail msg = Ty (Left msg)

instance Applicative TypeChecker  where
    pure = return
    (<*>) = ap

instance Functor TypeChecker  where
    fmap f m = pure f <*> m 

instance Applicative Evaluator  where
    pure = return
    (<*>) = ap

instance Functor Evaluator  where
    fmap f m = pure f <*> m 

addValue::Value -> Value -> Value
addValue (IntV x) (IntV y) = IntV $ x + y
addValue _ _ = ErrorV "Undefined addition"


subValue::Value -> Value -> Value
subValue (IntV x) (IntV y) = IntV $ x - y
subValue _ _ = ErrorV "Undefined subtraction"

multValue::Value -> Value -> Value
multValue (IntV x) (IntV y) = IntV $ x * y
multValue _ _ = ErrorV "Undefined multiplication"

divValue::Value -> Value -> Value
divValue (IntV _) (IntV 0) = ErrorV "Cannot divide by zero"
divValue (IntV x) (IntV y) = IntV $ div x y
divValue _ _ = ErrorV "Undefined division"

modValue::Value -> Value -> Value
modValue (IntV _) (IntV 0) = ErrorV "Cannot mod by zero"
modValue (IntV x) (IntV y) = IntV $ mod x y
modValue _ _ = ErrorV "Undefined mod operation"

evaluatorToValue:: Evaluator Value -> Value
evaluatorToValue (Ev (Right v)) = v
evaluatorToValue (Ev (Left v)) = v


runArithmetic::Env -> (Value -> Value -> Value) -> AST -> AST -> Evaluator Value
runArithmetic env op leftArg rightArg =
    do
        leftEval <- runLang env leftArg
        rightEval <- runLang env rightArg
        return $ op leftEval rightEval

compareInt:: IntComparison -> Value -> Value -> Value
compareInt op (IntV l) (IntV r) = BoolV $ (toIntCompare op) l r
compareInt _ _ _ = ErrorV "Can only compare integers"

boolOp:: BoolOperation -> Value -> Value -> Value
boolOp op (BoolV l) (BoolV r) = BoolV $ (toBoolOp op) l r

eval :: AST -> Value
eval program = case typeCheck [] program of
                Ty (Right _)  -> case runLang [] program of
                            Ev (Left a) -> a
                            Ev (Right a) -> a
                Ty (Left msg) -> ErrorV $ "Typecheck error: " ++ msg

runLang :: Env -> AST -> Evaluator Value
runLang _ (IntT x) = return $ IntV x
runLang _ (BoolT x) = return $ BoolV x
runLang env (VarT x) = 
    case lookup x env of
            Just a -> return a
            Nothing -> fail $ "Undefined variable: " ++ x
runLang env (Add x y)  = runArithmetic env addValue x y
runLang env (Sub x y)  = runArithmetic env subValue x y
runLang env (Mult x y) = runArithmetic env multValue x y
runLang env (Div x y)  = runArithmetic env divValue x y
runLang env (Mod x y)  = runArithmetic env modValue x y

runLang env (If bool trueCase falseCase) = 
    do 
        boolEval <- runLang env bool
        case boolEval of 
            BoolV True -> runLang env trueCase
            BoolV False -> runLang env falseCase
            _ -> fail "Error, expected boolean"

-- To be completely honest - I did not expect this solution to work for a recursive let function
-- I was plesantly surprised when it worked out. Apparently lazy languages have a hell of an advantage
-- Note - I did think of doing it this way on my own. I wouldn't be surprised if there are similar
-- solutions online. I just tried a few things to try to avoid using the IO monad here
runLang env (Let varName varExpr body) =
    do
        runLang ((varName,argVal):env) body
    where argVal = evaluatorToValue $ runLang ((varName,argVal):env) varExpr


runLang env (LamT varName _ body) =
    return $ ClosureV varName body env

runLang env (App fun arg) =
    do
        funEval <- runLang env fun
        argEval <- runLang env arg
        runApp funEval argEval

runLang env (CompInt op left right) = 
    do
        leftEval <- runLang env left
        rightEval <- runLang env right
        return $ compareInt op leftEval rightEval

runLang env (BoolOp2 op left right) =
    do
        leftEval <- runLang env left
        rightEval <- runLang env right
        return $  boolOp op leftEval rightEval


runApp :: Value -> Value -> Evaluator Value
runApp (ClosureV varName body closEnv) argEvaluated =
    runLang ((varName,argEvaluated):closEnv) body
runApp _ _ = return $ ErrorV "Invalid application"


typeCheck :: Context -> AST -> TypeChecker Type
typeCheck _ (IntT _) = return IntType
typeCheck _ (BoolT _) = return BoolType
typeCheck cont (LamT argName (LamType argType returnType) body) =
    do
        bodyType <- typeCheck ((argName, argType):cont) body
        if bodyType == returnType
            then return $ LamType argType bodyType
            else fail $ "In lambda expected return::" ++ (show returnType) ++ " value does not match type of body::" ++ (show bodyType)
typeCheck cont (Add l r) = checkArithmeticType cont l r
typeCheck cont (Sub l r) = checkArithmeticType cont l r
typeCheck cont (Mult l r) = checkArithmeticType cont l r
typeCheck cont (Div l r) = checkArithmeticType cont l r
typeCheck cont (Mod l r) = checkArithmeticType cont l r
typeCheck cont (If bool trueResult falseResult) =
    do
        boolType  <- typeCheck cont bool
        trueType  <- typeCheck cont trueResult
        falseType <- typeCheck cont falseResult
        if (boolType == BoolType)
            then 
                if (trueType == falseType)
                    then return trueType
                    else fail $ "Need both branches of if statement to be of same type. True branch: " ++ (show trueType) ++ ". False branch: " ++ (show falseType)
            else
                fail $ "First argument to if statement must be boolean. Recieved: " ++ (show bool)

typeCheck cont (App func arg) = 
    do
        funcType <- typeCheck cont func
        argType  <- typeCheck cont arg
        typeCheckApp funcType argType

        -- case funcType of
        --      (LamType expectedArgType returnType) -> case argType of
        --                                                 actualArgType -> if actualArgType == expectedArgType 
        --                                                     then return returnType 
        --                                                     else fail $ "Expected: " ++ (show expectedArgType) ++ ", received: " ++ (show actualArgType)
        --     _ -> Nothing



typeCheck cont (VarT name) = 
    case lookup name cont of
        Just typ -> return typ
        Nothing -> fail $ "Could not find: " ++ name

typeCheck cont (Let name arg body) =
    do
        argType <- typeCheck cont arg
        typeCheck ((name,argType):cont) body
        
typeCheck cont (CompInt _ l r)
    = do
        leftType <- typeCheck cont l
        rightType <- typeCheck cont r
        if (leftType == IntType) && (rightType == IntType)
            then return BoolType
            else fail "Integer comparison requires two integer inputs"

typeCheck _ ast = fail $ "Cannot typecheck: " ++ (show ast)

checkArithmeticType:: Context -> AST -> AST -> TypeChecker Type
checkArithmeticType cont l r = 
    do
        leftType  <- typeCheck cont l
        rightType <- typeCheck cont r
        if (leftType == IntType) && (rightType == IntType)
            then return IntType 
            else fail $ "Cannot do arithmetic on: " ++ (show l)++ "::" ++ (show leftType) ++ " and " ++ (show r) ++ "::" ++ (show rightType)

typeCheckApp:: Type -> Type -> TypeChecker Type
typeCheckApp (LamType lamArgType lamReturnType) actualArgType = 
    if lamArgType == actualArgType
        then return lamReturnType
        else fail $ "Expected argument type of: " ++ show lamArgType ++ " received: " ++ show actualArgType
typeCheckApp nonLamType _ = fail $ "Expected function, received: " ++ show nonLamType