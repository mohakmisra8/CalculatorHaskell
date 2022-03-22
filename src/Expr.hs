module Expr where

import Parsing

import Data.Either
import Data.Text (splitOn)
import Data.Map 
import qualified Data.Map as Map

type Name = String

removeJust :: Maybe a -> a
removeJust (Just a ) = a

removeMaybe :: Either a b -> b
removeMaybe (Right a) = a

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
          | Str String
          | Bool Bool 
          | Lit Lit
          | Fac Expr
          | Mod Expr Expr
          | Abs Expr
          | And Expr Expr
          | Implies Expr Expr
          | Or Expr Expr
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

data Lit = IntVal Int | StrVal String | BoolVal Bool
  deriving (Show, Eq)

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
eval vars (Bool b) = Right (Just (BoolVal b)) -- for values, just give the value directly
eval vars (Var n) = Right $ Just (findVar vars n) -- look-up actual value of variable
eval vars (Add x y) = Right $ Just (IntVal (getVal vars x + getVal vars y)) -- implemented by DEEPANKUR
eval vars (Sub x y) = Right $ Just (IntVal (getVal vars x - getVal vars y)) -- implemented by DEEPANKUR
eval vars (Div x y) = Right $ Just (IntVal (intDiv vars x y)) -- implemented by DEEPANKUR
eval vars (Mult x y) = Right $ Just (IntVal (getVal vars x * getVal vars y)) -- implemented by DEEPANKUR
eval vars (Pow x y) = Right $ Just (IntVal (getVal vars x ^ getVal vars y)) -- implemented by DEEPANKUR
eval vars (Fac x) = Right $ Just (IntVal (factorial vars x))
eval vars (Mod x y) = Right $ Just (IntVal ( mod (getVal vars x) (getVal vars y) ))--MOHAK
eval vars (ToString x) = Right $ Just (StrVal (show x))
eval vars (And a b) = Right $ Just (BoolVal (getBool vars a && getBool vars b)) -- implemented by DEEPANKUR

getBool :: Map Name Lit -> Expr -> Bool
getBool vars (Bool b) = b
getBool vars expr = toBool vars (eval vars expr)

findVar :: Map Name Lit -> Name -> Lit --find index
findVar stack n = IntVal $ findIndex n stack 

intDiv :: Map Name Lit -> Expr -> Expr -> Int
intDiv vars a b = div (getVal vars a) (getVal vars b)

getVal :: Map Name Lit -> Expr -> Int
getVal vars (Val a) = a
getVal vars expr = toInt vars (eval vars expr)

toInt :: Map Name Lit -> Either Error (Maybe Lit) -> Int
toInt vars result | isLeft result = error "not integer"
                  | otherwise = getVal vars (getExpr (removeJust (removeMaybe result)))

toBool :: Map Name Lit -> Either Error (Maybe Lit) -> Bool
toBool vars result | isLeft result = error "not boolean"
                  | otherwise = getBool vars (getExpr (removeJust (removeMaybe result)))


getLit :: Expr -> Lit
getLit (Lit a) = a
getLit (Val a) = IntVal a
getLit (Str a) = StrVal a
getLit _       = IntVal 0

getExpr :: Lit -> Expr
getExpr (IntVal a) = Val a
getExpr (StrVal a) = Str a
getExpr _       = Val 0

-- prints out Str "variable_name" or Val number rather than "variable_name" or number
litToString :: Lit -> String
litToString (StrVal a) = a
litToString (IntVal a) = litToString (StrVal (show a))
litToString (BoolVal a) = litToString (StrVal (show a))

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'
--Mohak
factorial :: Map Name Lit -> Expr -> Int
factorial _ (Val 0) = 1
factorial vars n =  getVal vars n * factorial vars (Val (getVal vars n - 1))

ass                         :: Parser (Name, Expr)
ass                         =  do n <- token ident
                                  char '='
                                  eq <- algebra
                                  return (n, eq)
                               ||| do (a, i) <- ass_int
                                      return (a, Val i)
                                ||| do (a, s) <- ass_str
                                       return (a, Str s)
                                ||| do (a, b) <- ass_bool
                                       return (a, b)

strToBool :: String -> Expr
strToBool s | elem s ["T", "true", "True", "1"] = Bool True 
            | elem s ["F", "false", "False", "0"] = Bool False

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
                                  ||| do a <- token integer
                                         õ <- pow 
                                         b <- algebra
                                         return (op õ  (Val a) b)  
                                  ||| do a <- token integer
                                         õ <- timesOrDivide 
                                         b <- algebra
                                         return (op õ  (Val a) b)
                                  ||| do a <- token integer
                                         õ <- plusOrMinus
                                         b <- algebra
                                         return (op õ  (Val a) b)
                                  ||| do a <- token integer
                                         õ <- plusOrMinus
                                         b <- token integer
                                         return (op õ (Val a) (Val b))
                                  ||| do a <- token integer
                                         õ <- timesOrDivide
                                         b <- token integer
                                         return (op õ (Val a) (Val b))
                                  ||| do a <- token integer
                                         õ <- pow
                                         b <- token integer
                                         return (op õ (Val a) (Val b))
                                  ||| do n <- token clause
                                         return (n)
                                  ||| do n <- token integer 
                                         return (Val n)

ass_bool                         :: Parser (String, Expr)
ass_bool                         =  do a <- token ident
                                       char '='
                                       b <- token boolean
                                       return (a, b)

boolean :: Parser Expr
boolean = do a <- token bool_literal
             string "||"
             b <- token boolean
             return (Or (strToBool a) b)
      ||| do a <- token bool_exp
             string "||"
             b <- token bool_exp
             return (Or a b)
      ||| do a <- token bool_literal
             string "->"
             b <- token boolean
             return (Implies (strToBool a) b)
      ||| do a <- token bool_exp
             string "->"
             b <- token bool_exp
             return (Implies a b)
      ||| do a <- token bool_literal
             string "&&"
             b <- token boolean
             return (And (strToBool a) b)
      ||| do a <- token bool_exp
             string "&&"
             b <- token bool_exp
             return (And a b)
      ||| do b <- token bool_exp
             return (b)
      


bool_exp :: Parser Expr
bool_exp = do char '('
              b <- boolean
              char ')'
              return (b)
      ||| do b <- token bool_literal
             return (strToBool b)

op :: Char -> Expr -> Expr ->Expr
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

clause :: Parser Expr
clause =     do n <- token integer 
                õ <- fac 
                return (sop õ  (Val n))
         ||| do char '('
                a <- algebra
                char ')'
                return a
         ||| do char '|'
                a <- algebra
                char '|'
                return (sop '|'  a)
 

pCommand :: Parser Command
pCommand = do (t, content) <- ass
              return (Set t content)
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