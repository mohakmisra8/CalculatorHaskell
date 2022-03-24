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
          | Same Expr Expr
          | Less Expr Expr
          | Greater Expr Expr
          | Not Expr
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | While Expr [Command] -- evaluate an expression and run a series of commands while it is true
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
eval vars (Or a b) = Right $ Just (BoolVal (getBool vars a || getBool vars b)) -- implemented by DEEPANKUR 
eval vars (Implies a b) = Right $ Just (BoolVal (getBool vars (And (Not a) b))) -- implemented by DEEPANKUR 

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

ass_bool                         :: Parser (String, Expr)
ass_bool                         =  do a <- token ident
                                       char '='
                                       b <- token boolean
                                       return (a, b)

boolean :: Parser Expr
boolean = do a <- token bool_literal
             token (string "||")
             b <- token boolean
             return (Or (strToBool a) b)
      ||| do a <- token bool_exp
             token (string "||")
             b <- token bool_exp
             return (Or a b)
      ||| do a <- token bool_literal
             token (string "->")
             b <- token boolean
             return (Implies (strToBool a) b)
      ||| do a <- token bool_exp
             token (string "->")
             b <- token bool_exp
             return (Implies a b)
      ||| do a <- token bool_literal
             token (string "&&")
             b <- token boolean
             return (And (strToBool a) b)
      ||| do a <- token bool_exp
             token (string "&&")
             b <- token bool_exp
             return (And a b)
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
                        ||| do int <- token integer
                               return (Val int)

value_bool                      :: Parser Expr
value_bool                    =  do name <- token ident 
                                    return (Var name)
                        ||| do bl <- token bool_literal 
                               return (strToBool bl)
  

pCommand :: Parser Command
pCommand = do (cond, body) <- while
              return (While cond body)
        |||do (t, content) <- ass
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