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
          | Fact Expr
          | Mod Expr
          | Abs Expr
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

data Lit = IntVal Int | StrVal String
  deriving Show

eval :: [(Name, Lit)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Lit -- Result (if no errors such as missing variables)
eval vars (Val x) = Just (IntVal x) -- for values, just give the value directly
eval vars (Str s) = Just (StrVal s) -- for values, just give the value directly
eval vars (Var n) = Just (findVar vars n) -- look-up actual value of variable
eval vars (Add x y) = Just (IntVal (getVal x + getVal y)) -- implemented by DEEPANKUR
eval vars (Sub x y) = Just (IntVal (getVal x - getVal y)) -- implemented by DEEPANKUR
eval vars (Mod x y) = Just (IntVal (getVal x % getVal y)) -- implemented by DEEPANKUR
eval vars (Div x y) = Just (IntVal (intDiv x y)) -- implemented by DEEPANKUR
eval vars (Mult x y) = Just (IntVal (getVal x * getVal y)) -- implemented by DEEPANKUR
eval vars (Pow x y) = Just (IntVal (getVal x ^ getVal y)) -- implemented by DEEPANKUR
eval vars (Fact x) = Just (IntVal (factorial x))--MOHAK
eval vars (Abs x) = Just (IntVal (abs x))--MOHAK
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

-- prints out Str "variable_name" or Val number rather than "variable_name" or number
litToString :: Lit -> String
litToString (StrVal a) = a

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'
--Mohak
factorial :: Expr -> Int
--factorial 0 = 1
factorial n =  getVal n * factorial (Val (getVal n - 1))
--Mohak
abs:: Expr -> Int
abs n | getVal n >= 0 = getVal n
      | otherwise = - getVal n 

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
algebra                         = do a <- token clause
                                     char '+'
                                     b <- token clause
                                     return (Add a b)  
                                  ||| do a <- token integer
                                         char '/'
                                         b <- algebra
                                         return (Div (Val a) b)
                                  ||| do a <- token integer
                                         õ <- plusOrMinus
                                         b <- algebra
                                         return ((op õ) (Val a) b)
                                  ||| do a <- algebra
                                         õ <- plusOrMinus
                                         b <- token integer
                                         return ((op õ) a (Val b))
                                  ||| do a <- token integer
                                         õ <- plusOrMinus
                                         b <- token integer
                                         return ((op õ) (Val a) (Val b))
                                  ||| do a <- token integer
                                         char '/'
                                         b <- token integer
                                         return (Div (Val a) (Val b))
                                  ||| do a <- token integer
                                         char '%'--added this, check if it works
                                         b <- token integer
                                         return (Mod (Val a) (Val b))

op :: Char -> Expr -> Expr -> Expr
op '+' = Add
op '-' = Sub

clause :: Parser Expr
clause = do char '('
            a <- algebra
            char ')'
            return a

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              Set [t] <$> pExpr
            ||| do string "print"
                   space
                   Print <$> pExpr
--done by MOHAK
pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              Add t <$> pExpr
            ||| do char '-'
                   Sub t <$> pExpr
                 ||| return t
--MOHAK
pFactor :: Parser Expr
pFactor = do Val . digitToInt <$> digit
           ||| do v <- letter
                  e <- pExpr
                  return (Fact e)
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e
--MOHAK
pTerm :: Parser Expr
pTerm = do f <- pFactor
           e <- pExpr
           do char '*'--multiplication
              t <- pTerm
              e <- pExpr
              return (Mult t e)
            ||| do char '/'
                   t <- pTerm
                   e <- pExpr
                   return (Div t e) 
                 ||| return f