module Expr where

import Parsing

type Name = String

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
          | Lit Lit
          | Fac Expr
          | Mod Expr Expr
          | Abs Expr
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

data Lit = IntVal Int | StrVal String
  deriving (Show, Eq)

data Error 
       = UnknownOperationError Char 
       | SingleOperationError Char
       deriving Show
              

eval :: [(Name, Lit)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        --Lit
        --Either Error Lit -- Result (if no errors such as missing variables)
eval vars (Val x) = Just (IntVal x) -- for values, just give the value directly
eval vars (Str s) = Just (StrVal s) -- for values, just give the value directly
eval vars (Var n) = Just (findVar vars n) -- look-up actual value of variable
eval vars (Add x y) = Just (IntVal (getVal x + getVal y)) -- implemented by DEEPANKUR
eval vars (Sub x y) = Just (IntVal (getVal x - getVal y)) -- implemented by DEEPANKUR
eval vars (Div x y) = Just (IntVal (intDiv x y)) -- implemented by DEEPANKUR
eval vars (Mult x y) = Just (IntVal (getVal x * getVal y)) -- implemented by DEEPANKUR
eval vars (Pow x y) = Just (IntVal (getVal x ^ getVal y)) -- implemented by DEEPANKUR
eval vars (Fac x) = Just (IntVal (factorial x))
eval vars (Mod x y) = Just (IntVal ( mod (getVal x) (getVal y) ))--MOHAK
eval vars (ToString x) = Just (StrVal (show x))

findVar :: [(Name, Lit)] -> Name -> Lit
findVar stack n | fst (stack!!0) == n  = snd (head stack)
                | otherwise = findVar (drop 1 stack) n

intDiv :: Expr -> Expr -> Int
intDiv a b = div (getVal a) (getVal b)

getVal :: Expr -> Int
getVal (Val a) = a

getLit :: Expr -> Lit
getLit (Lit a) = a
getLit (Val a) = IntVal a
getLit (Str a) = StrVal a
getLit _       = IntVal 0

-- prints out Str "variable_name" or Val number rather than "variable_name" or number
litToString :: Lit -> String
litToString (StrVal a) = a
litToString (IntVal a) = litToString (StrVal (show a))

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'
--Mohak
factorial :: Expr -> Int
--factorial 0 = 1
factorial n =  getVal n * factorial (Val (getVal n - 1))

ass                         :: Parser (Name, Expr)
ass                         =  do n <- token ident
                                  char '='
                                  eq <- algebra
                                  return (n, eq)
                               ||| do (a, i) <- ass_int
                                      return (a, Val i)
                                ||| do (a, s) <- ass_str
                                       return (a, Str s)

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