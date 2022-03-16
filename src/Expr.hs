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
          | Fact Expr
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

data Lit = IntVal Int | StrVal String 

eval :: [(Name, Lit)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Lit -- Result (if no errors such as missing variables)
eval vars (Val x) = Just (IntVal x) -- for values, just give the value directly
eval vars (Str s) = Just (StrVal s) -- for values, just give the value directly
eval vars (Var n) = Just (findVar vars n) -- look-up actual value of variable
eval vars (Add x y) = Just (IntVal (getVal x + getVal y)) -- implemented by DEEPANKUR
eval vars (Sub x y) = Just (IntVal (getVal x - getVal y)) -- implemented by DEEPANKUR
eval vars (Div x y) = Just (IntVal (intDiv x y)) -- implemented by DEEPANKUR
eval vars (Mult x y) = Just (IntVal (getVal x * getVal y)) -- implemented by DEEPANKUR
eval vars (Pow x y) = Just (IntVal (getVal x ^ getVal y)) -- implemented by DEEPANKUR
eval vars (Fact x) = Just ((IntVal) factorial(getVal x))
eval vars (ToString x) = Just (StrVal (show x))

findVar :: [(Name, Lit)] -> Name -> Lit
findVar stack n | (fst (stack!!0)) == n  = snd (stack!!0)
                | otherwise = findVar (drop 1 stack) n

intDiv :: Expr -> Expr -> Int
intDiv a b = div (getVal a) (getVal b) 

getVal :: Expr -> Int
getVal (Val a) = a 

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

factorial :: Expr -> Int
factorial 0 = 1
factorial n =  (getVal) n * factorial (n - 1)

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              e <- pExpr
              return (Set [t] e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
--done by MOHAK
pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Sub t e)  
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
           ||| do v <- letter
                  return factorial(d)
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Mult t e)
            ||| do char '/'
                   t <- pTerm
                   return (Div t e) 
                 ||| return f