{-# LANGUAGE GADTs, KindSignatures, InstanceSigs, OverloadedStrings  #-}
module Lang where
    
import GHC.Exts()

type Env = [(String, Value)]
type Context = [(String, Type)]

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
    deriving (Show, Eq)

addValue::Value -> Value -> Value
addValue (IntV x) (IntV y) = IntV $ x + y

subValue::Value -> Value -> Value
subValue (IntV x) (IntV y) = IntV $ x - y

multValue::Value -> Value -> Value
multValue (IntV x) (IntV y) = IntV $ x * y

divValue::Value -> Value -> Value
divValue (IntV x) (IntV 0) = ErrorV "Cannot divide by zero"
divValue (IntV x) (IntV y) = IntV $ x / y

modValue::Value -> Value -> Value
modValue (IntV x) (IntV 0) = ErrorV "Cannot mod by zero"
modValue (IntV x) (IntV y) = IntV $ mod x y


data AST :: * where
    BoolT    :: Bool -> AST
    IntT     :: Int  -> AST
    VarT     :: String -> AST
    LamT     :: AST -> Type -> AST -> AST           --VarT, Body
    If       :: AST -> AST -> AST -> AST
    Add      :: AST -> AST -> AST
    Mult     :: AST -> AST -> AST
    Sub      :: AST -> AST -> AST
    Div      :: AST -> AST -> AST
    Mod      :: AST -> AST -> AST
    Let      :: String -> AST -> AST -> AST
    App      :: AST -> AST -> AST
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

runLang :: Env -> AST -> Value
runLang _ (IntT x) = IntV x
runLang _ (BoolT x) = BoolV x
runLang env (VarT x) = 
    case lookup x env of
        Just y -> y
        Nothing -> ErrorV ("No definition of variable: " ++ x)
runLang env (Add x y)  = addValue (runLang env x) (runLang env y)
runLang env (Sub x y)  = subValue (runLang env x) (runLang env y)
runLang env (Mult x y) = multValue (runLang env x) (runLang env y)
runLang env (Div x y)  = divValue (runLang env x) (runLang env y)
runLang env (Mod x y)  = modValue (runLang env x) (runLang env y)

runLang env (If bool trueCase falseCase) = 
    case (runLang env bool) of 
        BoolV True  -> runLang env trueCase
        BoolV False -> runLang env falseCase

runLang env (Let varName varExpr body) =
    runLang ((varName,runLang env varExpr):env) body

runLang env (LamT (VarT varName) argType body) =
    ClosureV varName body env

runLang env (App fun arg) =
    runApp (runLang env fun) (runLang env arg)

runApp :: Value -> Value -> Value
runApp (ClosureV varName body closEnv) argEvaluated =
    runLang ((varName,argEvaluated):closEnv) body

typeCheck :: Context -> AST -> (Maybe Type)
typeCheck _ (IntT _) = Just IntType
typeCheck _ (BoolT _) = Just BoolType
typeCheck cont (LamT (VarT argName) argType body) =
    do
        bodyType <- typeCheck ((argName, argType):cont) body
        return $ LamType argType bodyType

typeCheck cont (Add l r) = checkArithmeticType cont l r
typeCheck cont (Sub l r) = checkArithmeticType cont l r
typeCheck cont (Mult l r) = checkArithmeticType cont l r
typeCheck cont (Div l r) = checkArithmeticType cont l r
typeCheck cont (Mod l r) = checkArithmeticType cont l r


typeCheck cont (VarT name) = 
    lookup name cont

typeCheck cont (If bool trueResult falseResult) =
    do
        boolType  <- typeCheck cont bool
        trueType  <- typeCheck cont trueResult
        falseType <- typeCheck cont falseResult
        if (boolType == BoolType) && (trueType == falseType)
            then return trueType
            else Nothing

typeCheck cont (App func arg) = 
    do
        funcType <- typeCheck cont func
        argType  <- typeCheck cont arg
        case funcType of
            (LamType expectedArgType returnType) -> case argType of
                                                        expectedArgType -> return returnType
            otherwise -> Nothing

typeCheck cont (Let name arg body) =
    do
        argType <- typeCheck cont arg
        bodyType <- typeCheck ((name,argType):cont) body
        return bodyType

typeCheck _ _ = Nothing

checkArithmeticType cont l r = 
    do
        leftType  <- typeCheck cont l
        rightType <- typeCheck cont r
        if (leftType == IntType) && (rightType == IntType)
            then return IntType 
            else Nothing
