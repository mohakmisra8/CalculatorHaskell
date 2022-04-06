module Expr where

import Parsing

import Data.Either
import Data.Text (splitOn)
import Data.Map
import qualified Data.Map as Map
import System.Directory (setAccessTime)

type Name = String

--removes the Just before the value
removeJust :: Maybe a -> a
removeJust (Just a ) = a

--removes the Either to extract teh maybe Lit
--converts Error to Maybe Lit so that it can be printed out
removeMaybe :: Either Error (Maybe Lit) -> (Maybe Lit)
removeMaybe (Right a) = a
removeMaybe (Left a) = Just (StrVal (show a))

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
             | Def Name [Expr] [Command]
             | Call Name [Expr]
             | If Expr [Command]
  deriving Show

--variables will be of these types
data Lit = IntVal Int | StrVal String | BoolVal Bool | FloatVal Float
  deriving (Show, Eq, Ord)

--most of our error will be returned like this so that the program continues running
data Error
       = IncorrectUsageError [Char]
       deriving Show

--eval method evaluetes the expressions passed in and returns either an error or a Lit value
eval :: Map Name Lit -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either Error (Maybe Lit) -- Result (if no errors such as missing variables)
eval vars (Val x) = Right (Just (IntVal x)) -- for values, just give the value directly
eval vars (Str s) = Right (Just (StrVal s)) -- for values, just give the value directly
eval vars (FVal f)= Right (Just (FloatVal f)) -- for values, just give the value directly
eval vars (Bool b) = Right (Just (BoolVal b)) -- for values, just give the value directly
eval vars (Var n) = Right $ Just (findVar vars n) -- look-up actual value of variable

--adds to values together, or concatenates two strings
                           --compares based on the type of x when it is evaluated by eval
eval vars (Add x y) = case getExprType vars x of
                                 --if it is a "Val" or "FVal" check that y is also a "Val" or "FVal" and add them together
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (IntVal (getVal vars x + getVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x + getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x + getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x + getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"
                                --if it is a string check that y is also a string, and concatenate them
                                "String" -> case getExprType vars y of
                                                     "String"  -> Right $ Just (StrVal ((getStrVal vars x) ++ (getStrVal vars y)))
                                                     otherwise -> Left $ IncorrectUsageError "cannot concatenate a non string"
                                -- if x is a boolean return an error
                                otherwise -> Left $ IncorrectUsageError "Cannot add boolean values"

--subtracts the values, checking the types of x and y as above
eval vars (Sub x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (IntVal (getVal vars x - getVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x - getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x - getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x - getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

--multiplies the values, checking the types of x and y as above
eval vars (Mult x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (IntVal (getVal vars x * getVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x * getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x * getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x * getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

--divides the values, checking the types of x and y as above
eval vars (Div x (Val 0)) = Left $ IncorrectUsageError "cannot divide by 0"
eval vars (Div x (FVal 0.0)) = Left $ IncorrectUsageError "cannot divide by 0"
eval vars (Div x y) = case getExprType vars x of
                                "Val" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"
                                "FVal" -> case getExprType vars y of
                                                     "Val"     -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     "FVal"    -> Right $ Just (FloatVal (getFloatVal vars x / getFloatVal vars y))
                                                     otherwise -> Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

--if x is less than 0 and y is not an integer return 0
--if y is greater than 0 return x to the power of y
--if x is 0 return 0
--return x to the power of y
--(-1) ** 0.5 doesnt work but (-1) ** 2 works
-- ** same as ^ but for floats
eval vars (Pow x y) | getFloatVal vars x < 0 && (not $ isInt (getFloatVal vars y)) = Right (Just (IntVal 0))
                    | getFloatVal vars y > 0  = Right $ Just (FloatVal (getFloatVal vars x ** getFloatVal vars y))
                    | getFloatVal vars x == 0 = Right (Just (IntVal 0))
                    | otherwise               = Right (Just (FloatVal $ 1/(getFloatVal vars x ** (abs $ getFloatVal vars y))))

--returns the factorial of x if x is a Val when evaluated and is greater than 0
eval vars (Fac x) | getExprType vars x == "Val" && toInt vars (removeJust  (removeMaybe  (eval vars x))) >= 0 = Right (Just $ IntVal (factorial vars x))
                  | otherwise                   = Left $ IncorrectUsageError "cannot apply factorial to a non int value"

--returns x % y when both x and y evaluate to "Val" and y is not 0
eval vars (Mod x y) | getExprType vars x == "Val" && getExprType vars y == "Val" && getVal vars y /= 0 = Right (Just $ IntVal $ mod (getVal vars x) (getVal vars y))
                    | otherwise                                                                        = Left $ IncorrectUsageError "can only perform the mudulus function on ints"

--converts a to a string
eval vars (ToString a) = Right $ Just (StrVal (getStrVal vars a))

--convertd a Val or a FVal to a Val, otherwise returns an error
eval vars (ToInt a)    = case getExprType vars a of
                                   "Val" -> eval vars a
                                   "FVal"-> Right $ Just (IntVal $ getVal vars a)
                                   otherwise -> Left $ IncorrectUsageError "cannot convert string or bool to int"

--returns the result of a && b if a and b are "Bool"s, returns an error otherwise
eval vars (And a b) | getExprType vars a == "Bool" && getExprType vars b == "Bool" = Right $ Just (BoolVal (getBool vars a && getBool vars b))
                    | otherwise                                                    = Left $ IncorrectUsageError "Cannot perform boolean comparisons on non boolean variables"

--returns the result of a || b if a and b are "Bool"s, returns an error otherwise
eval vars (Or a b) | getExprType vars a == "Bool" && getExprType vars b == "Bool" = Right $ Just (BoolVal (getBool vars a || getBool vars b))
                   | otherwise                                                    = Left $ IncorrectUsageError "Cannot perform boolean comparisons on non boolean variables"

--returns the result of a -> b if a and b are "Bool"s, returns an error otherwise
eval vars (Implies a b) | getExprType vars a == "Bool" && getExprType vars b == "Bool" = Right $ Just (BoolVal (not (getBool vars a) || getBool vars b))
                        | otherwise                                                    = Left $ IncorrectUsageError "Cannot perform boolean comparisons on non boolean variables"

--returns the boolean resulting from x < y, if the types of x and y are types that can be compared
eval vars (Less x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getFloatVal vars x < getFloatVal vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getFloatVal vars x < getFloatVal vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getFloatVal vars x < getFloatVal vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getFloatVal vars x < getFloatVal vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "String" -> case getExprType vars x of
                                                 "String" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "Bool"-> case getExprType vars x of
                                                 "Bool" -> Right $ Just (BoolVal (getLit vars x < getLit vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"

--returns the boolean resulting from x > y, if the types of x and y are types that can be compared
eval vars (Greater x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getFloatVal vars x > getFloatVal vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getFloatVal vars x > getFloatVal vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getFloatVal vars x > getFloatVal vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getFloatVal vars x > getFloatVal vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "String" -> case getExprType vars x of
                                                 "String" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "Bool"-> case getExprType vars x of
                                                 "Bool" -> Right $ Just (BoolVal (getLit vars x > getLit vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"

--returns the boolean resulting from x == y, if the types of x and y are types that can be compared
eval vars (Same x y) = case getExprType vars y of
                                 "Val" -> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getFloatVal vars x == getFloatVal vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getFloatVal vars x == getFloatVal vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "FVal"-> case getExprType vars x of
                                                 "Val" -> Right $ Just (BoolVal (getFloatVal vars x == getFloatVal vars y))
                                                 "FVal"-> Right $ Just (BoolVal (getFloatVal vars x == getFloatVal vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "String" -> case getExprType vars x of
                                                 "String" -> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"
                                 "Bool"-> case getExprType vars x of
                                                 "Bool" -> Right $ Just (BoolVal (getLit vars x == getLit vars y))
                                                 otherwise -> Left $ IncorrectUsageError "cannot perform comparison between two different types"

--returns the boolean resulting from not a, if a is a boolean when evaluated
eval vars (Not a) | (getExprType vars a) == "Bool" = Right $ Just (BoolVal $ not (getBool vars (getExpr (removeJust $ removeMaybe $ eval vars a))))
                  | otherwise               = Left $ IncorrectUsageError "Cannot perform boolean comparisons on non boolean variables"

--performs trigonometric functions on values if they match certain conditions
eval vars (Sin a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (sin (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Cos a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (cos (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Tan a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (tan (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Arcsin a) | (getExprType vars a == "Val" || getExprType vars a == "FVal") && getFloatVal vars a <= 1 && getFloatVal vars a >= -1 = Right $ Just (FloatVal (asin (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Arccos a) | (getExprType vars a == "Val" || getExprType vars a == "FVal") && getFloatVal vars a <= 1 && getFloatVal vars a >= -1 = Right $ Just (FloatVal (acos (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Arctan a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (atan (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Sinh a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (sinh (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Cosh a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (cosh (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Tanh a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (tanh (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Arcsinh a) | getExprType vars a == "Val" || getExprType vars a == "FVal" = Right $ Just (FloatVal (asinh (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Arccosh a) | (getExprType vars a == "Val" || getExprType vars a == "FVal") && getFloatVal vars a <= 1 && getFloatVal vars a >= -1 = Right $ Just (FloatVal (acosh (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Arctanh a) | (getExprType vars a == "Val" || getExprType vars a == "FVal") && getFloatVal vars a <= 1 && getFloatVal vars a >= -1 = Right $ Just (FloatVal (atanh (getFloatVal vars a)))
                  | otherwise = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

eval vars (Abs a) | getExprType vars a == "FVal" = Right $ Just (FloatVal (abs (getFloatVal vars a)))
                  | getExprType vars a == "Val"  = Right $ Just (IntVal (abs (getVal vars a)))
                  | otherwise                    = Left $ IncorrectUsageError "cannot perform mathematical operations on a non number"

--returns the type that the Expr gets evaluated down to as a string
getExprType :: Map Name Lit -> Expr -> String
getExprType vars a  = toType (removeJust $ removeMaybe $ eval vars a)

--returns the Type of a Lit passed in
toType :: Lit -> String
toType (IntVal a)   = "Val"
toType (FloatVal a) = "FVal"
toType (BoolVal a)  = "Bool"
toType (StrVal a)   = "String"

--returns a bool from a given expression
getBool :: Map Name Lit -> Expr -> Bool
getBool vars (Bool b) = b
getBool vars expr = toBool vars (eval vars expr)

toBool :: Map Name Lit -> Either Error (Maybe Lit) -> Bool
toBool vars result | isLeft result = error "not boolean"
                   | otherwise = getBool vars (getExpr (removeJust (removeMaybe result)))

--finds a variable in the map of variables
findVar :: Map Name Lit -> Name -> Lit
findVar stack n = removeJust (Data.Map.lookup n stack)

--returns a float from a given expression, also converts a Val to a FVal
getFloatVal :: Map Name Lit -> Expr -> Float
getFloatVal vars (FVal a) = a
getFloatVal vars (Val a)  = (fromIntegral a)::Float
getFloatVal vars a  = toFloat vars (removeJust $ removeMaybe $ eval vars a)

--used to convert an int to float
toFloat :: Map Name Lit -> Lit -> Float
toFloat vars (IntVal a) = (fromIntegral a)::Float
toFloat vars (FloatVal a) = a
toFloat vars l       = error "cannot convert to int"

--returns a string from a given expression
getStrVal :: Map Name Lit -> Expr -> String
getStrVal vars (Str a) = a
getStrVal vars a = toString (removeJust $ removeMaybe (eval vars a))

toString :: Lit -> String
toString (IntVal a)   = show a
toString (FloatVal a) = show a
toString (BoolVal a)  = show a
toString (StrVal a)   = a

--returns an int from a given expression
getVal :: Map Name Lit -> Expr -> Int
getVal vars (Val a) = a
getVal vars (FVal a) = fromInteger (round a)
getVal vars x = toInt vars (removeJust $ removeMaybe (eval vars x))

toInt :: Map Name Lit -> Lit -> Int
toInt vars (IntVal a) = a
toInt vars (FloatVal a)= fromInteger (round a)
toInt vars l       = error "cannot convert to int"

--returns true if the Float can be converted to a int without changing its value
isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

--gets a lit from an expression
getLit :: Map Name Lit -> Expr -> Lit
getLit vars (Lit a) = a
getLit vars (Val a) = IntVal a
getLit vars (Str a) = StrVal a
getLit vars (FVal a)= FloatVal a
getLit vars (Bool a)= BoolVal a
getLit vars a = removeJust $ removeMaybe $ eval vars a

--gets an expression from a lit
getExpr :: Lit -> Expr
getExpr (IntVal a)  = Val a
getExpr (StrVal a)  = Str a
getExpr (BoolVal a) = Bool a
getExpr (FloatVal a)= FVal a

--returns the factorial value as an int
factorial :: Map Name Lit -> Expr -> Int
factorial _ (Val 0) = 1
factorial vars n =  getVal vars n * factorial vars (Val (getVal vars n - 1))

--parser returns a command
pCommand2 :: Parser Command
pCommand2 = do (name, args, body) <- assign_func
               return (Def name args body)
              ||| do (name, vars) <- call_func
                     return (Call name vars)
              ||| do (cond, body) <- while
                     return (While cond body)
              ||| do command <- Expr.repeat
                     return command
              ||| do (cond, body) <- if_comd
                     return (If cond body)
              ||| do (t, content) <- assign
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

--parses in a function definition
assign_func :: Parser (String, [Expr], [Command])
assign_func = do  char ':'
                  func <- token ident
                  args <- token tuple
                  char '='
                  token (string "{")
                  body <- many pCommand2
                  token (string "}")
                  return (func, args, body)

--parses in a function call
call_func :: Parser (Name, [Expr])
call_func = do char ':'
               n <- token ident
               a <- token tuple
               return (n, a)

--parses in a while loop
while :: Parser (Expr, [Command])
while = do char '?'
           cond <- token boolean2
           char '?'
           token (string "<<")
           body <- many pCommand2
           token (string ">>")
           return (cond, body)

--parses in a repeat loop
repeat :: Parser Command
repeat = do token (char '#')
            num <- token int
            space
            char '{'
            body <- many pCommand2
            char '}'
            return (Repeat num body)

--parses in a if command
if_comd :: Parser (Expr, [Command])
if_comd = do char '?'
             cond <- token boolean2
             space
             char '{'
             body <- many pCommand2
             char '}'
             return (cond, body)

--parses in variable assignment
assign                         :: Parser (Name, Expr)
assign                         =  do n <- token ident
                                     char '='
                                     eq <- algebra3
                                     return (n, eq)
                                ||| do (a, s) <- assign_str
                                       return (a, Str s)
                                ||| do (a, b) <- assign_bool
                                       return (a, b)

--parses in a boolean assignment
assign_bool                         :: Parser (String, Expr)
assign_bool                         =  do a <- token ident
                                          char '='
                                          b <- token boolean2
                                          return (a, b)

--parses in boolean operators
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

--parses in more boolean stuff (3<4) etc
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

--parses in values in an algebraic order
algebra3                           :: Parser Expr
algebra3                           = do a <- token clause2
                                        char '!'--case for factortial
                                        return (Fac a)
                                     ||| do a <- token trig--for trig
                                            return a
                                     ||| do a <- token clause2--for power
                                            char '^'
                                            b <- token clause2
                                            return (Pow a b)
                                     ||| do a <- token clause2--for multiplication
                                            char '*'
                                            b <- token clause2
                                            return (Mult a b)
                                     ||| do a <- token clause2
                                            char '/'-- for division
                                            b <- token clause2
                                            return (Div a b)
                                     ||| do a <- token clause2
                                            char '%'--for modulus
                                            b <- token clause2
                                            return (Mod a b)
                                     ||| do a <- token clause2
                                            char '+'--for addition
                                            b <- token clause2
                                            return (Add a b)
                                     ||| do a <- token clause2
                                            char '-'--for subrtractions
                                            b <- token clause2
                                            return (Sub a b)
                                     ||| do char '_'
                                            a <- token clause2
                                            return (ToString a)
                                     ||| do char '|'--for abs
                                            a <- token clause2
                                            return (ToInt a)
                                     ||| do a <- token clause2
                                            return a

--parses in trigonometric functions
trig :: Parser Expr
trig = do token $ string "asinh"--inverse sinh
          a <- token clause2
          return (Arcsinh a)
          ||| do token $ string "acosh"--inverse cosh
                 a <- token clause2
                 return (Arccosh a)
          ||| do token $ string "atanh"--inverse tanh
                 a <- token clause2
                 return (Arctanh a)
          ||| do token $ string "asin"--sin
                 a <- token clause2
                 return (Arcsin a)
          ||| do token $ string "acos"--inverse cos
                 a <- token clause2
                 return (Arccos a)
          ||| do token $ string "atan"--inverse tan
                 a <- token clause2
                 return (Arctan a)
          ||| do token $ string "sinh"--sinh
                 a <- token clause2
                 return (Sinh a)
          ||| do token $ string "cosh"--cosh
                 a <- token clause2
                 return (Cosh a)
          ||| do token $ string "tanh"--tannh
                 a <- token clause2
                 return (Tanh a)
          ||| do token $ string "sin"--sin
                 a <- token clause2
                 return (Sin a)
          ||| do token $ string "cos"--cos
                 a <- token clause2
                 return (Cos a)
          ||| do token $ string "tan"--tan
                 a <- token clause2
                 return (Tan a)

--parses in strings an numbers as well absolutes
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

--parses in a number of identifiers seperated by commas with the method just bellow   
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

--parses in multiple algebraic or text values with the method just bellow
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

--parses the bracketed part after a function can contain only variable names or any values
tuple :: Parser [Expr]
tuple = do token (char '(')
           vals <- multi_comma_seq
           token (char ')')
           return vals
           ||| do token $ char '('
                  vals <- multi_algebra3orText
                  token $ char ')'
                  return vals

--parses either algebra or a string
algebra3orText :: Parser Expr
algebra3orText = do a <- multi_str_cat
                    return (Str a)
                    ||| do a <- algebra3
                           return a

--parses the boolean values on either side of a boolean operator
getBools :: String -> Parser Expr
getBools s = do a <- token bool
                string s
                b <- getBools s
                return (op s a b)
                ||| do a <- token bool
                       return a
                
--returns an expression based on the value spassed in
op :: String -> Expr -> Expr -> Expr
op "&&" a b = And a b--and fucntion
op "||" a b = Or a b--or function
op "->" a b = Implies a b--implies

--returns an expression based on the value spassed in
dop :: String -> Expr -> Expr -> Expr
dop "==" a b = Same a b
dop ">" a b = Greater a b
dop "<" a b = Less a b
dop ">=" a b = Or (Greater a b) (Same a b)
dop "<=" a b = Or (Less a b) (Same a b)
dop "~=" a b = Not (Same a b)
