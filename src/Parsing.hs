{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Advanced parsers
-------------
-}



ass_int                         :: Parser (String, Int)
ass_int                         =  do a <- token ident
                                      char '='
                                      b <- token integer
                                      return (a, b)

ass_str                         :: Parser (String, String)
ass_str                         =  do a <- token ident
                                      char '='
                                      b <- token multi_str_cat
                                      return (a, b)

char_seq :: Parser String
char_seq = do char '"'
              x <- alphanumspace
              xs <- many alphanumspace
              char '"'
              return (x:xs)


str_cat :: Parser String
str_cat = do s1 <- token char_seq
             string "++"
             s2 <- token char_seq
             return (s1 ++ s2)
         ||| do s <- token char_seq
                return s

multi_str_cat :: Parser String
multi_str_cat = do s1 <- token str_cat
                   string "++"
                   s2 <- token str_cat
                   return (s1 ++ s2)
                ||| do s <- token str_cat
                       return s


{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

bigsat                           :: (String -> Bool) -> Parser String
bigsat p                         =  do x <- ident
                                       if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

alphanumspace                      :: Parser Char
alphanumspace                      =  sat isAlphaNumSpace

plusOrMinus                      :: Parser Char
plusOrMinus                      =  sat isPlusMinus

timesOrDivide                      :: Parser Char
timesOrDivide                      =  sat isTimesDivide

pow                      :: Parser Char
pow                      =  sat isPow

fac                      :: Parser Char
fac                      =  sat isFac

isPlusMinus :: Char -> Bool
isPlusMinus c = elem c ['+', '-']

isTimesDivide :: Char -> Bool
isTimesDivide c = elem c ['*', '/']

isPow :: Char -> Bool
isPow '^' = True
isPow c = False

isFac :: Char -> Bool
isFac '!' = True
isFac c = False

isAlphaNumSpace :: Char -> Bool
isAlphaNumSpace ' ' = True 
isAlphaNumSpace c = isAlphaNum c 

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)