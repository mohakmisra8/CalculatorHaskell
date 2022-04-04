module Expr where

import Parsing

import Data.Either
import Data.Text (splitOn)
import Data.Map
import qualified Data.Map as Map

type Name = String
type Type = String

removeJust :: Maybe a -> a
removeJust (Just a ) = a

removeMaybe :: Either a b -> b
removeMaybe (Right a) = a
removeMaybe (Left a) = error "An error has occured"

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mult Expr Expr
          | Pow Expr Expr
          | ToString Expr
          | Var Name
          | Val Int
          | FVal Float
          | Str String
          | Bool Bool
          | Lit Lit
          | Fac Expr
          | Mod Expr Expr
          | Abs Expr
          | And Expr Expr
          | Implies Expr Expr
          | Or Expr Expr
          | Same Expr Expr
          | Less Expr Expr
          | Greater Expr Expr
          | Not Expr
          | Sin Expr 
          | Cos Expr 
          | Tan Expr 
          | Arcsin Expr 
          | Arccos Expr 
          | Arctan Expr 
          | Sinh Expr 
          | Cosh Expr 
          | Tanh Expr 
          | Arcsinh Expr 
          | Arccosh Expr 
          | Arctanh Expr 
          | Exp Expr 
  deriving (Show, Eq, Ord)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | While Expr [Command]
             | Repeat Int [Command] -- evaluate an expression and run a series of commands while it is true
             | Def Type Name [Expr] [Command]
             | Return Expr
  deriving Show

data Lit = IntVal Int | StrVal String | BoolVal Bool | FloatVal Float 
  deriving (Show, Eq, Ord)

data Error
       = UnknownOperationError Char
       | SingleOperationError Char
       deriving Show


eval :: Map Name Lit -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        --Lit
        Either Error (Maybe Lit) -- Result (if no errors such as missing variables)
eval vars (Val x) = Right (Just (IntVal x)) -- for values, just give the value directly
eval vars (Str s) = Right (Just (StrVal s)) -- for values, just give the value directly
eval vars (FVal f)= Right (Just (FloatVal f))
eval vars (Bool b) = Right (Just (BoolVal b)) -- for values, just give the value directly
eval vars (Var n) = Right $ Just (findVar vars n) -- look-up actual value of variable
eval vars (Add (Val x) (Val y)) = Right $ Just (IntVal (getVal vars (Val x) + getVal vars (Val y)))
eval vars (Add (Var x) (Val y)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) + getVal vars (Val y))) 
                                | otherwise                                                 = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) + getFloatVal vars (Val y)))
eval vars (Add (Val y) (Var x)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) + getVal vars (Val y))) 
                                | otherwise                                                 = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) + getFloatVal vars (Val y))) 
eval vars (Add (Var x) (Var y)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) && isIntLit (removeJust $ removeMaybe $ (eval vars (Var y))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) + getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var y))))))) 
                                | otherwise                                                                                                              = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) + getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var y))))))) 
eval vars (Add (FVal x) (Val y)) = Right $ Just (FloatVal (getFloatVal vars (FVal x) + getFloatVal vars (Val y))) -- implemented by DEEPANKUR
eval vars (Add (Val x) (FVal y)) = Right $ Just (FloatVal (getFloatVal vars (Val x) + getFloatVal vars (FVal y)))
eval vars (Add (FVal x) (FVal y)) = Right $ Just (FloatVal (getFloatVal vars (FVal x) + getFloatVal vars (FVal y)))
eval vars (Add _ _) = Left $ error "cannot perform mathematical operations on a non number"
--error for not numbers
eval vars (Sub (Val x) (Val y)) = Right $ Just (IntVal (getVal vars (Val x) - getVal vars (Val y))) -- implemented by DEEPANKUR
eval vars (Sub (Var x) (Val y)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) - getVal vars (Val y))) 
                                | otherwise                                                 = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) - getFloatVal vars (Val y)))
eval vars (Sub (Val y) (Var x)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) = Right $ Just (IntVal (getVal vars (Val (getVal vars (Val y) - (toInt vars (removeJust $ removeMaybe (eval vars (Var x)))))))) 
                                | otherwise                                                 = Right $ Just (FloatVal (getFloatVal vars (Val y) - (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x)))))))) 
eval vars (Sub (Var x) (Var y)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) && isIntLit (removeJust $ removeMaybe $ (eval vars (Var y))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) - getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var y))))))) 
                                | otherwise                                                                                                              = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) - getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var y))))))) 
eval vars (Sub (FVal x) (Val y)) = Right $ Just (FloatVal (getFloatVal vars (FVal x) - getFloatVal vars (Val y))) -- implemented by DEEPANKUR
eval vars (Sub (Val x) (FVal y)) = Right $ Just (FloatVal (getFloatVal vars (Val x) - getFloatVal vars (FVal y)))
eval vars (Sub (FVal x) (FVal y)) = Right $ Just (FloatVal (getFloatVal vars (FVal x) - getFloatVal vars (FVal y)))
eval vars (Sub _ _) = Left $ error "cannot perform mathematical operations on a non number"
--error for not numbers
eval vars (Div x (Val 0)) = Right (Just (IntVal 0))
eval vars (Div x (FVal 0)) = Right (Just (IntVal 0))
eval vars (Div x y) = Right $ Just (FloatVal ((getFloatVal vars x)/(getFloatVal vars y))) -- implemented by DEEPANKUR
--error for not numbers
eval vars (Mult (Val x) (Val y)) = Right $ Just (IntVal (getVal vars (Val x) * getVal vars (Val y)))
eval vars (Mult (Var x) (Val y)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) * getVal vars (Val y))) 
                                 | otherwise                                                 = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) * getFloatVal vars (Val y)))
eval vars (Mult (Val y) (Var x)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) = Right $ Just (IntVal (getVal vars (Val (getVal vars (Val y) * (toInt vars (removeJust $ removeMaybe (eval vars (Var x)))))))) 
                                 | otherwise                                                 = Right $ Just (FloatVal (getFloatVal vars (Val y) * (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x)))))))) 
eval vars (Mult (Var x) (Var y)) | isIntLit (removeJust $ removeMaybe $ (eval vars (Var x))) && isIntLit (removeJust $ removeMaybe $ (eval vars (Var y))) = Right $ Just (IntVal (getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var x))))) * getVal vars (Val (toInt vars (removeJust $ removeMaybe (eval vars (Var y))))))) 
                                 | otherwise                                                                                                              = Right $ Just (FloatVal (getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var x))))) * getFloatVal vars (FVal (toFloat vars (removeJust $ removeMaybe (eval vars (Var y))))))) 
eval vars (Mult (FVal x) (Val y)) = Right $ Just (FloatVal (getFloatVal vars (FVal x) * getFloatVal vars (Val y))) -- implemented by DEEPANKUR
eval vars (Mult (Val x) (FVal y)) = Right $ Just (FloatVal (getFloatVal vars (Val x) * getFloatVal vars (FVal y)))
eval vars (Mult (FVal x) (FVal y)) = Right $ Just (FloatVal (getFloatVal vars (FVal x) * getFloatVal vars (FVal y)))
eval vars (Mult _ _) = Left $ error "cannot perform mathematical operations on a non number"
--error for not numbers
--(-1) ** 0.5 doesnt work but (-1) ** 2 works
-- ** same as ^ but for floats
eval vars (Pow x y) | getFloatVal vars x < 0 && (not $ isInt (getFloatVal vars y)) = Right (Just (IntVal 0))
                    | getFloatVal vars y > 0  = Right $ Just (FloatVal (getFloatVal vars x ** getFloatVal vars y))
                    | getFloatVal vars x == 0 = Right (Just (IntVal 0))
                    | otherwise               = Right (Just (FloatVal $ 1/(getFloatVal vars x ** (abs $ getFloatVal vars y))))-- implemented by DEEPANKUR

eval vars (Fac (Val x)) | x >= 0    = Right $ Just (IntVal (factorial vars (Val x)))
                        | otherwise = Right (Just (IntVal 0))
eval vars (Mod (Val x) (Val 0)) = Right $ Just (IntVal 0)
eval vars (Mod (Val x) (Val y)) = Right $ Just (IntVal ( mod (getVal vars (Val x)) (getVal vars (Val y)) ))--MOHAK
eval vars (Mod _ _) = Right (Just (IntVal 0))
eval vars (ToString (Val x)) = Right $ Just (StrVal (show x))
eval vars (ToString (FVal x)) = Right $ Just (StrVal (show x))
eval vars (ToString (Bool x)) = Right $ Just (StrVal (show x))
eval vars (And (Bool a) (Bool b)) = Right $ Just (BoolVal (getBool vars (Bool a) && getBool vars (Bool b))) -- implemented by DEEPANKUR
eval vars (And a b) = Left $ error "Cannot perform boolean comparisons on non boolean variables"
eval vars (Or (Bool a) (Bool b)) = Right $ Just (BoolVal (getBool vars (Bool a) || getBool vars (Bool b))) -- implemented by DEEPANKUR
eval vars (Or a b) = Left $ error "Cannot perform boolean comparisons on non boolean variables"
eval vars (Implies (Bool a) (Bool b)) = Right $ Just (BoolVal (not a || b)) -- implemented by DEEPANKUR 
eval vars (Implies a b) = Left $ error "Cannot perform boolean comparisons on non boolean variables"


eval vars (Less x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "Str" -> case getExprType vars x of
                                                 "Str" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "Bool"-> case getExprType vars x of
                                                 "Bool" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"

eval vars (Greater x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "Str" -> case getExprType vars x of
                                                 "Str" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "Bool"-> case getExprType vars x of
                                                 "Bool" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"

eval vars (Same x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "Str" -> case getExprType vars x of
                                                 "Str" -> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "Bool"-> case getExprType vars x of
                                                 "Bool" -> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"


--eval vars (Greater (Val x) (Val y)) = Right $ Just (BoolVal (getLit (Val x) > getLit (Val y))) -- implemented by DEEPANKUR
--eval vars (Greater (FVal x) (FVal y)) = Right $ Just (BoolVal (getLit (FVal x) > getLit (FVal y)))
--eval vars (Greater (Bool x) (Bool y)) = Right $ Just (BoolVal (getLit (Bool x) > getLit (Bool y)))
--eval vars (Greater (Str x) (Str y)) = Right $ Just (BoolVal (getLit (Str x) > getLit (Str y)))
--eval vars (Greater _ _) = Left $ error "cannot perform comparison between two different types"
--eval vars (Same (Val x) (Val y)) = Right $ Just (BoolVal (getLit (Val x) == getLit (Val y))) -- implemented by DEEPANKUR
--eval vars (Same (FVal x) (FVal y)) = Right $ Just (BoolVal (getLit (FVal x) == getLit (FVal y)))
--eval vars (Same (Bool x) (Bool y)) = Right $ Just (BoolVal (getLit (Bool x) == getLit (Bool y)))
--eval vars (Same (Str x) (Str y)) = Right $ Just (BoolVal (getLit (Str x) == getLit (Str y)))
--eval vars (Same _ _) = Left $ error "cannot perform comparison between two different types"


eval vars (Not (Bool b)) = Right $ Just (BoolVal (not b)) -- implemented by DEEPANKUR
eval vars (Not a) | (getExprType vars a) == "Bool" = Right $ Just (BoolVal (getBool vars (getExpr (removeJust $ removeMaybe $ eval vars a))))
                        | otherwise               = Left $ error "Cannot perform boolean comparisons on non boolean variables"
eval vars (Sin (FVal x)) = Right $ Just (FloatVal (sin (getFloatVal vars (FVal x))))
eval vars (Sin (Val x)) = Right $ Just (FloatVal (sin (getFloatVal vars (Val x))))
eval vars (Sin x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Cos (FVal x)) = Right $ Just (FloatVal (cos (getFloatVal vars (FVal x))))
eval vars (Cos (Val x)) = Right $ Just (FloatVal (cos (getFloatVal vars (Val x))))
eval vars (Cos x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Tan (FVal x)) = Right $ Just (FloatVal (tan (getFloatVal vars (FVal x))))
eval vars (Tan (Val x)) = Right $ Just (FloatVal (tan (getFloatVal vars (Val x))))
eval vars (Tan x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Arcsin (FVal x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (asin (getFloatVal vars (FVal x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers outside -1 <= x <= 1"
eval vars (Arcsin (Val x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (asin (getFloatVal vars (Val x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers outside -1 <= x <= 1"
eval vars (Arcsin x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Arccos (FVal x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (acos (getFloatVal vars (FVal x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers outside -1 <= x <= 1"
eval vars (Arccos (Val x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (acos (getFloatVal vars (Val x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers outside -1 <= x <= 1"
eval vars (Arccos x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Arctan (FVal x)) = Right $ Just (FloatVal (atan (getFloatVal vars (FVal x))))
eval vars (Arctan (Val x)) = Right $ Just (FloatVal (atan (getFloatVal vars (Val x))))
eval vars (Arctan x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Sinh (FVal x)) = Right $ Just (FloatVal (sinh (getFloatVal vars (FVal x))))
eval vars (Sinh (Val x)) = Right $ Just (FloatVal (sinh (getFloatVal vars (Val x))))
eval vars (Sinh x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Cosh (FVal x)) = Right $ Just (FloatVal (cosh (getFloatVal vars (FVal x))))
eval vars (Cosh (Val x)) = Right $ Just (FloatVal (cosh (getFloatVal vars (Val x))))
eval vars (Cosh x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Tanh (FVal x)) = Right $ Just (FloatVal (tanh (getFloatVal vars (FVal x))))
eval vars (Tanh (Val x)) = Right $ Just (FloatVal (tanh (getFloatVal vars (Val x))))
eval vars (Tanh x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Arcsinh (FVal x)) = Right $ Just (FloatVal (asinh (getFloatVal vars (FVal x))))
eval vars (Arcsinh (Val x)) = Right $ Just (FloatVal (asinh (getFloatVal vars (Val x))))
eval vars (Arcsinh x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Arccosh (FVal x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (acosh (getFloatVal vars (FVal x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers inside -1 <= x <= 1"
eval vars (Arccosh (Val x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (acosh (getFloatVal vars (Val x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers inside -1 <= x <= 1"
eval vars (Arccosh x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Arctanh (FVal x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (atanh (getFloatVal vars (FVal x))))
                             | otherwise         = Left $ error "cannot perform this operation on numbers outside -1 <= x <= 1"
eval vars (Arctanh (Val x)) | x <= 1 && x >= -1 = Right $ Just (FloatVal (atanh (getFloatVal vars (Val x))))
                            | otherwise         = Left $ error "cannot perform this operation on numbers outside -1 <= x <= 1"
eval vars (Arctanh x) = Left $ error "cannot perform mathematical operations on a non number"
eval vars (Abs (FVal a)) = Right $ Just (FloatVal (abs a))
eval vars (Abs (Val a))  = Right $ Just (IntVal (abs a))
eval vars (Abs _)        = Left $ error "cannot perform mathematical operations on a non number"

getExprType :: Map Name Lit -> Expr -> String 
getExprType vars (Val a)  = "Val"
getExprType vars (FVal a) = "FVal"
getExprType vars (Bool a) = "Bool"
getExprType vars (Str a)  = "String"
getExprType vars a  = toType (removeJust $ removeMaybe $ eval vars a)

toType :: Lit -> String
toType (IntVal a)   = "Val"
toType (FloatVal a) = "FVal"
toType (BoolVal a)  = "Bool"
toType (StrVal a)   = "Str"

getBool :: Map Name Lit -> Expr -> Bool--implemented by Mohak 
getBool vars (Bool b) = b
getBool vars expr = toBool vars (eval vars expr)

findVar :: Map Name Lit -> Name -> Lit --find index implemented by Mohak 
findVar stack n = removeJust (Data.Map.lookup n stack)

intDiv :: Map Name Lit -> Expr -> Expr -> Float --implemented by Mohak 
intDiv vars a b = (getFloatVal vars a)/(getFloatVal vars b)

getFloatVal :: Map Name Lit -> Expr -> Float--implemented by Mohak 
getFloatVal vars (FVal a) = a
getFloatVal vars (Val a)  = (fromIntegral a)::Float
getFloatVal vars a  = toFloat vars ((removeJust $ removeMaybe $ eval vars a))
--getFloatVal vars expr     = toFloat vars (eval vars expr)

getVal :: Map Name Lit -> Expr -> Int--implemented by Mohak 
getVal vars (Val a) = a
getVal vars (FVal a) = fromInteger (round a)
getVal vars (Var x) = toInt vars (removeJust $ removeMaybe (eval vars (Var x)))
--getVal vars expr = toInt vars (removeJust $ removeMaybe (eval vars (Val x)))

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

isIntLit :: Lit -> Bool
isIntLit (IntVal a)   = True
isIntLit (FloatVal a) = isInt a
isIntLit _            = False

toInt :: Map Name Lit -> Lit -> Int--implemented by Mohak 
toInt vars (IntVal a) = a
toInt vars (FloatVal a)= fromInteger (round a)
toInt vars l       = error "cannot convert to int"

toFloat :: Map Name Lit -> Lit -> Float--implemented by Mohak 
toFloat vars (IntVal a) = (fromIntegral a)::Float
toFloat vars (FloatVal a) = a
toFloat vars l       = error "cannot convert to int"

toBool :: Map Name Lit -> Either Error (Maybe Lit) -> Bool--implemented by Mohak 
toBool vars result | isLeft result = error "not boolean"
                   | otherwise = getBool vars (getExpr (removeJust (removeMaybe result)))


getLit :: Map Name Lit -> Expr -> Lit
getLit vars (Lit a) = a
getLit vars (Val a) = IntVal a
getLit vars (Str a) = StrVal a
getLit vars (FVal a)= FloatVal a
getLit vars (Bool a)= BoolVal a
getLit vars a = removeJust $ removeMaybe $ eval vars a

getExpr :: Lit -> Expr
getExpr (IntVal a) = Val a
getExpr (StrVal a) = Str a
getExpr (BoolVal a) = Bool a
getExpr _       = Val 0

-- prints out Str "variable_name" or Val number rather than "variable_name" or number
litToString :: Lit -> String
litToString (StrVal a) = a
litToString (IntVal a) = litToString (StrVal (show a))
litToString (BoolVal a) = litToString (StrVal (show a))
litToString (FloatVal a) = litToString (StrVal (show a))

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'
--Mohak
factorial :: Map Name Lit -> Expr -> Int
factorial _ (Val 0) = 1
factorial vars n =  getVal vars n * factorial vars (Val (getVal vars n - 1))

ass                         :: Parser (Name, Expr)
ass                         =  do n <- token ident
                                  char '='
                                  eq <- algebra3
                                  return (n, eq)
                                ||| do (a, s) <- ass_str
                                       return (a, Str s)
                                ||| do (a, b) <- ass_bool
                                       return (a, b)
                                
ass_number                       :: Parser (String, Expr)
ass_number                       = do a <- token ident
                                      char '='
                                      b <- token float
                                      return (a, FVal b)
                                   ||| do a <- token ident
                                          char '='
                                          b <- token integer
                                          return (a, Val b)

                                      

strToBool :: String -> Expr
strToBool s | elem s ["$T", "$true", "$True", "$1"] = Bool True
            | elem s ["$F", "$false", "$False", "$0"] = Bool False

algebra                         :: Parser (Expr)
algebra                         =     do a <- token clause
                                         õ <- pow
                                         b <- token clause
                                         return (op õ  a b)
                                  ||| do a <- token clause
                                         õ <- timesOrDivide
                                         b <- token clause
                                         return (op õ  a b)
                                  ||| do a <- token clause
                                         õ <- plusOrMinus
                                         b <- token clause
                                         return (op õ  a b)
                                  ||| do a <- token value_int
                                         õ <- pow
                                         b <- algebra
                                         return (op õ  a b)
                                  ||| do a <- token value_int
                                         õ <- timesOrDivide
                                         b <- algebra
                                         return (op õ  a b)
                                  ||| do a <- token value_int
                                         õ <- plusOrMinus
                                         b <- algebra
                                         return (op õ  a b)
                                  ||| do a <- token value_int
                                         õ <- plusOrMinus
                                         b <- token value_int
                                         return (op õ a b)
                                  ||| do a <- token value_int
                                         õ <- timesOrDivide
                                         b <- token value_int
                                         return (op õ a b)
                                  ||| do a <- token value_int
                                         õ <- pow
                                         b <- token value_int
                                         return (op õ a b)
                                  ||| do n <- token clause
                                         return (n)
                                  ||| do n <- token value_int
                                         return n

algebra2                        :: Parser Expr
algebra2                        = do char '('
                                     a <- algebra2
                                     char ')'
                                     return a
                                  ||| do a <- token algebra2
                                         char '^'
                                         b <- token algebra2
                                         return (Pow a b)
                                   ||| do a <- token algebra2
                                          char '/'
                                          b <- token algebra2
                                          return (Div a b)
                                   ||| do a <- token algebra2
                                          char '*'
                                          b <- token algebra2
                                          return (Mult a b)
                                   ||| do a <- token algebra2
                                          char '-'
                                          b <- token algebra2
                                          return (Sub a b)
                                   ||| do a <- token algebra2
                                          char '+'
                                          b <- token algebra2
                                          return (Add a b)
                                   ||| do a <- token clause2
                                          return a

algebra3                           :: Parser Expr
algebra3                           = do char '('
                                        a <- clause2
                                        char ')'
                                        return a
                                     ||| do a <- token clause2
                                            char '^'
                                            b <- token clause2
                                            return (Pow a b)
                                     ||| do a <- token clause2
                                            char '*'
                                            b <- token clause2
                                            return (Mult a b)
                                     ||| do a <- token clause2
                                            char '/'
                                            b <- token clause2
                                            return (Div a b)
                                     ||| do a <- token clause2
                                            char '+'
                                            b <- token clause2
                                            return (Add a b)
                                     ||| do a <- token clause2
                                            char '-'
                                            b <- token clause2
                                            return (Sub a b)
                                     ||| do a <- token clause2
                                            return a

clause2                              :: Parser Expr
clause2                              = do char '('
                                          a <- algebra3
                                          char ')'
                                          return a
                                     ||| do a <- token ident
                                            return (Var a)
                                     ||| do a <- token float
                                            return (FVal a)
                                     ||| do a <- token integer 
                                            return (Val a)

                                         

ass_bool                         :: Parser (String, Expr)
ass_bool                         =  do a <- token ident
                                       char '='
                                       b <- token boolean
                                       return (a, b)

ass_func :: Parser (String, String, [Expr], [Command])
ass_func = do  ret_type <- token type_decl
               space
               func <- token ident
               args <- token tuple
               char '='
               token (string "{")
               body <- many pCommand
               token (string "}")
               return (ret_type, func, args, body)

comma_seq :: Parser [Expr]
comma_seq = do s1 <- token ident
               string ","
               s2 <- token ident
               return [Var s1, Var s2]
         ||| do s <- token ident
                return [Var s]

multi_comma_seq :: Parser [Expr]
multi_comma_seq = do s1 <- token comma_seq
                     string ","
                     s2 <- token comma_seq
                     return (s1 ++ s2)
                ||| do s <- token comma_seq
                       return s

tuple :: Parser [Expr]
tuple = do token (char '(')
           vals <- multi_comma_seq
           token (char ')')
           return vals
           

boolean :: Parser Expr
boolean = do a <- token bool_exp
             token (string "||")
             b <- token bool_exp
             return (Or a b)
      ||| do a <- token bool_exp
             token (string "->")
             b <- token bool_exp
             return (Implies a b)
      ||| do a <- token bool_exp
             token (string "&&")
             b <- token bool_exp
             return (And a b)
      ||| do a <- token bool_literal
             token (string "||")
             b <- token boolean
             return (Or (strToBool a) b)
      ||| do a <- token bool_literal
             token (string "->")
             b <- token boolean
             return (Implies (strToBool a) b)
      ||| do a <- token bool_literal
             token (string "&&")
             b <- token boolean
             return (And (strToBool a) b)
      ||| do a <- token bool_literal
             token (string "||")
             b <- token bool_literal
             return (Or (strToBool a) (strToBool b))
      ||| do a <- token bool_literal
             token (string "->")
             b <- token bool_literal
             return (Implies (strToBool a) (strToBool b))
      ||| do a <- token bool_literal
             token (string "&&")
             b <- token bool_literal
             return (And (strToBool a) (strToBool b))
      ||| do b <- token bool_exp
             return (b)

while :: Parser (Expr, [Command])
while = do char '?'
           cond <- token boolean
           char '?'
           token (string "<<")
           body <- many pCommand
           token (string ">>")
           return (cond, body)

repeat :: Parser Command
repeat = do token (char '#')
            num <- token int
            space
            char '{'
            body <- many pCommand
            char '}'
            return (Repeat num body)


bool_exp :: Parser Expr
bool_exp = do char '('
              b <- token boolean
              char ')'
              return (b)
      ||| do char '~'
             b <- token boolean
             return (Not b)
      ||| do b <- token bool_literal
             return (strToBool b)
      ||| do a <- algebra
             x <- comparator
             b <- algebra
             return (dop x a b)

op :: Char -> Expr -> Expr -> Expr
op '+' a b = Add a b
op '-' a b = Sub a b
op '*' a b = Mult a b
op '/' a b = Div a b
op '^' a b = Pow a b
op '%' a b = Mod a b
op _ _ _ = error "unknown operation"
--op _ _ _ = Left UnknownOperationError

sop :: Char -> Expr -> Expr
sop '!' n = Fac n
sop '|' n = Abs n
sop _ _ = error "unknown operation"
--sop _ _ = Left SingleOperationError

dop :: String -> Expr -> Expr -> Expr
dop "==" a b = Same a b
dop ">" a b = Greater a b
dop "<" a b = Less a b
dop ">=" a b = Or (Greater a b) (Same a b)
dop "<=" a b = Or (Less a b) (Same a b)
dop "~=" a b = Not (Same a b)
dop _ _ _ = error "unknown operation"
--dop _ _ = Left DoubleOperationError

clause :: Parser Expr
clause =     do n <- token value_int
                õ <- fac
                return (sop õ  n)
         ||| do char '('
                a <- algebra
                char ')'
                return a
         ||| do char '|'
                a <- algebra
                char '|'
                return (sop '|'  a)


value_int                      :: Parser Expr
value_int                    =  do name <- token ident
                                   return (Var name)
                        ||| do num <- token integer
                               return (Val num)

value_bool                      :: Parser Expr
value_bool                    =  do name <- token ident
                                    return (Var name)
                        ||| do bl <- token bool_literal
                               return (strToBool bl)


pCommand :: Parser Command
pCommand = do (ret_type, name, args, body) <- ass_func
              return (Def ret_type name args body)
           ||| do (cond, body) <- while
                  return (While cond body)
           |||do (t, content) <- ass
                 return (Set t content)
           |||do command <- Expr.repeat
                 return command
           ||| do string "print"
                  space
                  out_math <- algebra
                  return (Print out_math)
           ||| do string "print"
                  space
                  out_str <- multi_str_cat
                  return (Print (Str out_str))
           ||| do string "print"
                  space
                  out <- token ident
                  return (Print (Var out))
--done by MOHAK
pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              Add t <$> pExpr
            ||| do char '-'
                   Sub t <$> pExpr
                 ||| return t

pFactor :: Parser Expr
pFactor = do Val . digitToInt <$> digit
           ||| do v <- letter
                  e <- pExpr
                  return (Fac e)
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           e <- pExpr
           do char '*'
              t <- pTerm
              e <- pExpr
              return (Mult t e)
            ||| do char '/'
                   t <- pTerm
                   e <- pExpr
                   return (Div t e)
                 ||| return f

