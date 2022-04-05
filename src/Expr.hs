module Expr where

import Parsing

import Data.Either
import Data.Text (splitOn)
import Data.Map
import qualified Data.Map as Map
import System.Directory (setAccessTime)

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
          | ToInt Expr
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
             | Call Name [Expr]
             | If Expr [Command]
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

eval vars (Add x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (IntVal (getVal vars x + getVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x + getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x + getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x + getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"
                                "String" -> case getExprType vars y of
                                                     "String"  -> Right $ Just (StrVal ((getStrVal vars x) ++ (getStrVal vars y)))
                                                     otherwise -> Left $ error "cannot concatenate a non string"
                                otherwise -> Left $ error "Cannot add bolean values"

eval vars (Sub x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (IntVal (getVal vars x - getVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x - getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x - getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x - getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"

eval vars (Mult x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (IntVal (getVal vars x * getVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x * getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x * getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x * getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"

eval vars (Div x (Val 0)) = Left $ error "cannot divide by 0"
eval vars (Div x (FVal 0.0)) = Left $ error "cannot divide by 0"
eval vars (Div x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     otherwise -> Left $ error "cannot perform mathematical operations on a non number"



--error for not numbers
--(-1) ** 0.5 doesnt work but (-1) ** 2 works
-- ** same as ^ but for floats
eval vars (Pow x y) | getFloatVal vars x < 0 && (not $ isInt (getFloatVal vars y)) = Right (Just (IntVal 0))
                    | getFloatVal vars y > 0  = Right $ Just (FloatVal (getFloatVal vars x ** getFloatVal vars y))
                    | getFloatVal vars x == 0 = Right (Just (IntVal 0))
                    | otherwise               = Right (Just (FloatVal $ 1/(getFloatVal vars x ** (abs $ getFloatVal vars y))))-- implemented by DEEPANKUR

eval vars (Fac x) | getExprType vars x == "Val" && toInt vars (removeJust  (removeMaybe  (eval vars x))) >= 0 = Right (Just $ IntVal (factorial vars x))
                  | otherwise                   = Left $ error "cannot apply factorial to a non int value"

eval vars (Mod x y) | getExprType vars x == "Val" && getExprType vars y == "Val" && getVal vars y /= 0 = Right (Just $ IntVal $ mod (getVal vars x) (getVal vars y))
                    | otherwise                                                                        = Left $ error "can only perform the mudulus function on ints"
eval vars (ToString a) = Right $ Just (StrVal (getStrVal vars a))
eval vars (ToInt a)    = case getExprType vars a of
                                   "Val" -> eval vars a
                                   "FVal"-> Right $ Just (IntVal $ getVal vars a)
                                   otherwise -> Left $ error "cannot convert string or bool to int"
eval vars (And a b) | getExprType vars a == "Bool" && getExprType vars b == "Bool" = Right $ Just (BoolVal (getBool vars a && getBool vars b))
                    | otherwise                                                    = Left $ error "Cannot perform boolean comparisons on non boolean variables"
eval vars (Or a b) | getExprType vars a == "Bool" && getExprType vars b == "Bool" = Right $ Just (BoolVal (getBool vars a || getBool vars b))
                   | otherwise                                                    = Left $ error "Cannot perform boolean comparisons on non boolean variables"
eval vars (Implies a b) | getExprType vars a == "Bool" && getExprType vars b == "Bool" = Right $ Just (BoolVal (not (getBool vars a) || getBool vars b))
                        | otherwise                                                    = Left $ error "Cannot perform boolean comparisons on non boolean variables"


eval vars (Less x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ error "cannot perform comparison between two different types"
                                 "String" -> case getExprType vars x of
                                                 "String" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
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
                                 "String" -> case getExprType vars x of
                                                 "String" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
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


eval vars (Not a) | (getExprType vars a) == "Bool" = Right $ Just (BoolVal $ not (getBool vars (getExpr (removeJust $ removeMaybe $ eval vars a))))
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
eval vars (Abs a) | getExprType vars a == "FVal" = Right $ Just (FloatVal (abs (getFloatVal vars a)))
                  | getExprType vars a == "Val"  = Right $ Just (IntVal (abs (getVal vars a)))
                  | otherwise                    = Left $ error "cannot perform mathematical operations on a non number"

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
toType (StrVal a)   = "String"

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

getStrVal :: Map Name Lit -> Expr -> String
getStrVal vars (Str a) = a
getStrVal vars a = toString (removeJust $ removeMaybe (eval vars a))

toString :: Lit -> String
toString (IntVal a)   = show a
toString (FloatVal a) = show a
toString (BoolVal a)  = show a
toString (StrVal a)   = a

getVal :: Map Name Lit -> Expr -> Int--implemented by Mohak 
getVal vars (Val a) = a
getVal vars (FVal a) = fromInteger (round a)
getVal vars x = toInt vars (removeJust $ removeMaybe (eval vars x))
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

trig :: Parser Expr
trig = do token $ string "asinh"
          a <- token clause2
          return (Arcsinh a)
          ||| do token $ string "acosh"
                 a <- token clause2
                 return (Arccosh a)
          ||| do token $ string "atanh"
                 a <- token clause2
                 return (Arctanh a)
          ||| do token $ string "asin"
                 a <- token clause2
                 return (Arcsin a)
          ||| do token $ string "acos"
                 a <- token clause2
                 return (Arccos a)
          ||| do token $ string "atan"
                 a <- token clause2
                 return (Arctan a)
          ||| do token $ string "sinh"
                 a <- token clause2
                 return (Sinh a)
          ||| do token $ string "cosh"
                 a <- token clause2
                 return (Cosh a)
          ||| do token $ string "tanh"
                 a <- token clause2
                 return (Tanh a)
          ||| do token $ string "sin"
                 a <- token clause2
                 return (Sin a)
          ||| do token $ string "cos"
                 a <- token clause2
                 return (Cos a)
          ||| do token $ string "tan"
                 a <- token clause2
                 return (Tan a)

--TODO: add abs and trig functions, maybe slightly fix pow
algebra3                           :: Parser Expr
algebra3                           = do a <- token clause2
                                        char '!'
                                        return (Fac a)
                                     ||| do a <- token trig
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
                                            char '%'
                                            b <- token clause2
                                            return (Mod a b)
                                     ||| do a <- token clause2
                                            char '+'
                                            b <- token clause2
                                            return (Add a b)
                                     ||| do a <- token clause2
                                            char '-'
                                            b <- token clause2
                                            return (Sub a b)
                                     ||| do char '_'
                                            a <- token clause2
                                            return (ToString a)
                                     ||| do char '|'
                                            a <- token clause2
                                            return (ToInt a)
                                     ||| do a <- token clause2
                                            return a

clause2                              :: Parser Expr
clause2                              = do char '('
                                          a <- algebra3
                                          char ')'
                                          return a
                                     ||| do char '|'
                                            a <- algebra3
                                            char '|'
                                            return (Abs a)
                                     ||| do a <- token ident
                                            return (Var a)
                                     ||| do a <- token float
                                            return (FVal a)
                                     ||| do a <- token integer 
                                            return (Val a)
                                     ||| do a <- token multi_str_cat 
                                            return (Str a)

                                         

ass_bool                         :: Parser (String, Expr)
ass_bool                         =  do a <- token ident
                                       char '='
                                       b <- token boolean2
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

multi_algebra3orText :: Parser [Expr]
multi_algebra3orText = do a <- token multiAlgebra3OrText
                          char ','
                          b <- token multiAlgebra3OrText
                          return (a ++ b)
                         ||| do a <- token multiAlgebra3OrText
                                return a

multiAlgebra3OrText :: Parser [Expr]
multiAlgebra3OrText = do a <- token algebra3orText
                         char ','
                         b <- token algebra3orText
                         return [a, b]
                         ||| do a <- token algebra3orText
                                return [a]

tuple :: Parser [Expr]
tuple = do token (char '(')
           vals <- multi_comma_seq
           token (char ')')
           return vals
           ||| do token $ char '('
                  vals <- multi_algebra3orText
                  token $ char ')'
                  return vals
           
boolean2 :: Parser Expr
boolean2 = do a <- token bool
              string "||"
              b <- token $ getBools "||"
              return (Or a b)
              ||| do a <- token bool
                     string "&&"
                     b <- getBools "&&"
                     return (And a b)
              ||| do a <- token bool
                     string "->"
                     b <- getBools "->"
                     return (Implies a b)
              ||| do a <- token bool
                     return a

bool :: Parser Expr
bool = do char '('
          a <- boolean2
          char ')'
          return a
          ||| do char '~'
                 a <- bool
                 return (Not a)
          ||| do a <- token bool_literal 
                 return (Bool a)
          ||| do a <- algebra3orText
                 x <- comparator
                 b <- algebra3orText
                 return (dop x a b)
          ||| do a <- algebra3
                 return a

algebra3orText :: Parser Expr
algebra3orText = do a <- multi_str_cat
                    return (Str a)
                    ||| do a <- algebra3
                           return a

getBools :: String -> Parser Expr
getBools s = do a <- token bool
                string s
                b <- getBools s
                return (Or a b)
                ||| do a <- token bool
                       return a
                

while :: Parser (Expr, [Command])
while = do char '?'
           cond <- token boolean2
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


pCommand2 :: Parser Command
pCommand2 = do (ret_type, name, args, body) <- ass_func
               return (Def ret_type name args body)
              ||| do (name, vars) <- call_func
                     return (Call name vars)
              ||| do (cond, body) <- while
                     return (While cond body)
              ||| do command <- Expr.repeat
                     return command
              ||| do (cond, body) <- if_comd
                     return (If cond body)
              ||| do (t, content) <- ass
                     return (Set t content)
              ||| do string "print"
                     space
                     out_math <- algebra3
                     return (Print out_math)
              ||| do string "print"
                     space
                     out_str <- multi_str_cat
                     return (Print (Str out_str))
              ||| do string "print"
                     space
                     out <- token ident
                     return (Print (Var out))

if_comd :: Parser (Expr, [Command])
if_comd = do char '?'
             cond <- token boolean2
             space
             char '{'
             body <- many pCommand
             char '}'
             return (cond, body)

call_func :: Parser (Name, [Expr])
call_func = do char ':'
               n <- token ident
               a <- token tuple
               return (n, a)