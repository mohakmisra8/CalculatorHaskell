module TestFile_QuickCheck where

--imports go here
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Map
import qualified Data.Map as Map
import Data.Either

import REPL
import Expr

--Tests Go Here

testVariables = [("variable1", (IntVal 10000)),("variable2", (StrVal "variable_data")),("variable3", (BoolVal True)),("variable4", (FloatVal 3.8e-3)),("variable5", (IntVal 100)),("variable6", (StrVal "15")),("variable7", (BoolVal False))]

testMap :: Map Name Lit
testMap = fromList testVariables

--checks that integer values can be added to a variable
--prop_testUpdateVars1 :: String -> Int -> Bool
--prop_testUpdateVars1 var val = updateVars var (IntVal val) [] == returnList
  --                             where returnList = [(var, (IntVal val))]

--checks that string value can be added to a variable
--prop_testUpdateVars2 :: String -> String -> Bool
--prop_testUpdateVars2 var val = updateVars var (StrVal val) [] == returnList
  --                             where returnList = [(var, (StrVal val))]

--checks that integer values stored in variables can be updated
--prop_testUpdateVars3 :: String -> Int -> Int -> Bool
--prop_testUpdateVars3 var val1 val2 = updateVars var (IntVal val2) (updateVars var (IntVal val1) []) == returnList
  --                                   where returnList = [(var, (IntVal val2))]

--checks that string values stored in variables can be updated
--prop_testUpdateVars4 :: String -> String -> String -> Bool
--prop_testUpdateVars4 var val1 val2 = updateVars var (StrVal val2) (updateVars var (StrVal val1) []) == returnList
  --                                   where returnList = [(var, (StrVal val2))]

--checks that a variable storing an integer can be updated to store a string
--prop_testUpdateVars5 :: String -> Int -> String -> Bool
--prop_testUpdateVars5 var val1 val2 = updateVars var (StrVal val2) (updateVars var (IntVal val1) []) == returnList
  --                                   where returnList = [(var, (StrVal val2))]

--checks that a variable storing an integer can be updated to store a string
--prop_testUpdateVars6 :: String -> String -> Int -> Bool
--prop_testUpdateVars6 var val1 val2 = updateVars var (IntVal val2) (updateVars var (StrVal val1) []) == returnList
  --                                   where returnList = [(var, (IntVal val2))]


----------------------------------------------------------------------
--                  Test the eval methods                           --
----------------------------------------------------------------------

prop_testEvalValx :: Int -> Bool
prop_testEvalValx x = (IntVal x) == removeJust (removeMaybe evalReturn)
                      where evalReturn = eval testMap (Val x)

prop_testEvalStrs :: String -> Bool
prop_testEvalStrs s = (StrVal s) == removeJust(removeMaybe evalReturn)
                      where evalReturn = eval testMap (Str s)

prop_testEvalFVal :: Float -> Bool
prop_testEvalFVal f = (FloatVal f) == removeJust(removeMaybe evalReturn)
                      where evalReturn = eval testMap (FVal f)

prop_testEvalBoolb :: Bool -> Bool
prop_testEvalBoolb b = (BoolVal b) == removeJust( removeMaybe evalReturn)
                       where evalReturn = eval testMap (Bool b)

prop_testEvalVarn :: Int -> Bool
prop_testEvalVarn n = output == removeJust (removeMaybe evalReturn)
                      where num = (abs n) `mod` 7
                            name = fst (testVariables !! num)
                            evalReturn = eval testMap (Var name)
                            output = snd (testVariables !! num)

prop_testEvalAddxy :: Int -> Int -> Bool
prop_testEvalAddxy x y = IntVal (x + y) == removeJust (removeMaybe evalReturn)
                         where evalReturn = eval testMap (Add (Val x) (Val y))

prop_testEvalAddxy1Float :: Int -> Float -> Bool
prop_testEvalAddxy1Float x y = FloatVal (a + y) == removeJust (removeMaybe evalReturn)
                               where evalReturn = eval testMap (Add (Val x) (FVal y))
                                     a = (fromIntegral x)::Float

prop_testEvalAddxy2Floats :: Float -> Float -> Bool
prop_testEvalAddxy2Floats x y = FloatVal (x + y) == removeJust (removeMaybe evalReturn)
                                where evalReturn = eval testMap (Add (FVal x) (FVal y))

prop_testEvalSubxy :: Int -> Int -> Bool
prop_testEvalSubxy x y = IntVal (x - y) == removeJust (removeMaybe evalReturn)
                         where evalReturn = eval testMap (Sub (Val x) (Val y))

prop_testEvalSubxy1Float :: Int -> Float -> Bool
prop_testEvalSubxy1Float x y = FloatVal (a - y) == removeJust (removeMaybe evalReturn)
                               where evalReturn = eval testMap (Sub (Val x) (FVal y))
                                     a = (fromIntegral x)::Float

prop_testEvalSubxy2Floats :: Float -> Float -> Bool
prop_testEvalSubxy2Floats x y = FloatVal (x - y) == removeJust (removeMaybe evalReturn)
                                where evalReturn = eval testMap (Sub (FVal x) (FVal y))

prop_testEvalDivxy :: Int -> Int -> Bool
prop_testEvalDivxy x 0 = isLeft evalReturn
                         where evalReturn = eval testMap (Div (Val x) (Val 0))
prop_testEvalDivxy x y = FloatVal (a/b) == removeJust (removeMaybe evalReturn)
                         where a = (fromIntegral x)::Float
                               b = (fromIntegral y)::Float
                               evalReturn = eval testMap (Div (Val x) (Val y))

prop_testEvalDivxy1Float :: Int -> Float -> Bool
prop_testEvalDivxy1Float x 0 = isLeft evalReturn
                               where evalReturn = eval testMap (Div (Val x) (FVal 0))
prop_testEvalDivxy1Float x y = FloatVal (a/y) == removeJust (removeMaybe evalReturn)
                               where a = (fromIntegral x)::Float
                                     evalReturn = eval testMap (Div (Val x) (FVal y))

prop_testEvalDivxy2Float :: Float -> Float -> Bool
prop_testEvalDivxy2Float x y | y == 0 = isLeft evalReturn
                             | otherwise = FloatVal (x/y) == removeJust (removeMaybe evalReturn)
                                           where evalReturn = eval testMap (Div (FVal x) (FVal y))

prop_testEvalMultxy :: Int -> Int -> Bool
prop_testEvalMultxy x y = IntVal (x * y) == removeJust (removeMaybe evalReturn)
                          where evalReturn = eval testMap (Mult (Val x) (Val y))

prop_testEvalMultxy1Float :: Int -> Float -> Bool
prop_testEvalMultxy1Float x y = FloatVal (a * y) == removeJust (removeMaybe evalReturn)
                                where a = (fromIntegral x)::Float
                                      evalReturn = eval testMap (Mult (Val x) (FVal y))

prop_testEvalMultxy2Float :: Float -> Float -> Bool
prop_testEvalMultxy2Float x y = FloatVal (x * y) == removeJust (removeMaybe evalReturn)
                                where evalReturn = eval testMap (Mult (FVal x) (FVal y))

prop_testEvalPowxy :: Int -> Int -> Bool
prop_testEvalPowxy x y | y > 0     = FloatVal (a ** b) == removeJust (removeMaybe evalReturn)
                       | x == 0    = IntVal 0       == removeJust (removeMaybe evalReturn)
                       | otherwise = FloatVal (1/(a ** (abs b))) == removeJust (removeMaybe evalReturn)
                         where evalReturn = eval testMap (Pow (Val x) (Val y))
                               a = (fromIntegral x)::Float
                               b = (fromIntegral y)::Float

prop_testEvalPowxyFloatBase :: Float -> Int -> Bool
prop_testEvalPowxyFloatBase x y | y > 0     = FloatVal (x ** b) == removeJust (removeMaybe evalReturn)
                                | x == 0    = IntVal 0       == removeJust (removeMaybe evalReturn)
                                | otherwise = FloatVal (1/(x ** (abs b))) == removeJust (removeMaybe evalReturn)
                                  where evalReturn = eval testMap (Pow (FVal x) (Val y))
                                        b = (fromIntegral y)::Float

prop_testEvalPowxyFloatExponent :: Int -> Float -> Bool
prop_testEvalPowxyFloatExponent x y | x < 0 && (not $ isInt y) = IntVal 0 == removeJust (removeMaybe evalReturn)
                                    | y > 0     = FloatVal (a ** y) == removeJust (removeMaybe evalReturn)
                                    | x == 0    = IntVal 0 == removeJust (removeMaybe evalReturn)
                                    | otherwise = FloatVal (1/(a ** (abs y))) == removeJust (removeMaybe evalReturn)
                                      where evalReturn = eval testMap (Pow (Val x) (FVal y))
                                            a = (fromIntegral x)::Float

prop_testEvalPowxy2FLoat :: Float -> Float -> Bool
prop_testEvalPowxy2FLoat x y | x < 0 && (not $ isInt y) = IntVal 0 == removeJust (removeMaybe evalReturn)
                             | y > 0     = FloatVal (x ** y) == removeJust (removeMaybe evalReturn)
                             | x == 0    = IntVal 0 == removeJust (removeMaybe evalReturn)
                             | otherwise = FloatVal (1/(x ** (abs y))) == removeJust (removeMaybe evalReturn)
                               where evalReturn = eval testMap (Pow (FVal x) (FVal y))

prop_testFacx :: Int -> Bool
prop_testFacx x | x >= 0    = IntVal (factorial testMap (Val x)) == removeJust (removeMaybe evalReturn)
                | otherwise = isLeft evalReturn
                  where evalReturn = eval testMap (Fac (Val x))

prop_testModxy :: Int -> Int -> Bool
prop_testModxy x 0 = isLeft evalReturn
                     where evalReturn = eval testMap (Mod (Val x) (Val 0))
prop_testModxy x y = IntVal (x `mod` y) == removeJust (removeMaybe evalReturn)
                     where evalReturn = eval testMap (Mod (Val x) (Val y))

prop_evalIntToString :: Int -> Bool
prop_evalIntToString x = StrVal (show x) == removeJust (removeMaybe evalReturn)
                         where evalReturn = eval testMap (ToString (Val x))

prop_evalFloatToString :: Float -> Bool
prop_evalFloatToString x = StrVal (show x) == removeJust (removeMaybe evalReturn)
                           where evalReturn = eval testMap (ToString (FVal x))

prop_evalBoolToString :: Bool -> Bool
prop_evalBoolToString x = StrVal (show x) == removeJust (removeMaybe evalReturn)
                          where evalReturn = eval testMap (ToString (Bool x))

prop_evalAnd :: Bool -> Bool -> Bool
prop_evalAnd a b = BoolVal (a && b) == removeJust (removeMaybe evalReturn)
                   where evalReturn = eval testMap (And (Bool a) (Bool b))

prop_evalOr :: Bool -> Bool -> Bool
prop_evalOr a b = BoolVal (a || b) == removeJust (removeMaybe evalReturn)
                  where evalReturn = eval testMap (Or (Bool a) (Bool b))

prop_evalImplies :: Bool -> Bool -> Bool
prop_evalImplies a b = BoolVal (not a || b) == removeJust (removeMaybe evalReturn)
                       where evalReturn = eval testMap (Implies (Bool a) (Bool b))

prop_evalLessThanInt :: Int -> Int -> Bool
prop_evalLessThanInt x y = BoolVal (x < y) == removeJust (removeMaybe evalReturn)
                           where evalReturn = eval testMap (Less (Val x) (Val y))

prop_evalLessThanFloat :: Float -> Float -> Bool
prop_evalLessThanFloat x y = BoolVal (x < y) == removeJust (removeMaybe evalReturn)
                             where evalReturn = eval testMap (Less (FVal x) (FVal y))

prop_evalLessThanBool :: Bool -> Bool -> Bool
prop_evalLessThanBool x y = BoolVal (x < y) == removeJust (removeMaybe evalReturn)
                            where evalReturn = eval testMap (Less (Bool x) (Bool y))

prop_evalLessThanStr :: String -> String -> Bool
prop_evalLessThanStr x y = BoolVal (x < y) == removeJust (removeMaybe evalReturn)
                           where evalReturn = eval testMap (Less (Str x) (Str y))

prop_evalGreaterThanInt :: Int -> Int -> Bool
prop_evalGreaterThanInt x y = BoolVal (x > y) == removeJust (removeMaybe evalReturn)
                              where evalReturn = eval testMap (Greater (Val x) (Val y))

prop_evalGreaterThanFloat :: Float -> Float -> Bool
prop_evalGreaterThanFloat x y = BoolVal (x > y) == removeJust (removeMaybe evalReturn)
                                where evalReturn = eval testMap (Greater (FVal x) (FVal y))

prop_evalGreaterThanBool :: Bool -> Bool -> Bool
prop_evalGreaterThanBool x y = BoolVal (x > y) == removeJust (removeMaybe evalReturn)
                               where evalReturn = eval testMap (Greater (Bool x) (Bool y))

prop_evalGreaterThanStr :: String -> String -> Bool
prop_evalGreaterThanStr x y = BoolVal (x > y) == removeJust (removeMaybe evalReturn)
                              where evalReturn = eval testMap (Greater (Str x) (Str y))

prop_evalSameInt :: Int -> Int -> Bool
prop_evalSameInt x y = BoolVal (x == y) == removeJust (removeMaybe evalReturn)
                              where evalReturn = eval testMap (Same (Val x) (Val y))

prop_evalSameFloat :: Float -> Float -> Bool
prop_evalSameFloat x y = BoolVal (x == y) == removeJust (removeMaybe evalReturn)
                                where evalReturn = eval testMap (Same (FVal x) (FVal y))

prop_evalSameBool :: Bool -> Bool -> Bool
prop_evalSameBool x y = BoolVal (x == y) == removeJust (removeMaybe evalReturn)
                               where evalReturn = eval testMap (Same (Bool x) (Bool y))

prop_evalSameStr :: String -> String -> Bool
prop_evalSameStr x y = BoolVal (x == y) == removeJust (removeMaybe evalReturn)
                              where evalReturn = eval testMap (Same (Str x) (Str y))

prop_evalNot :: Bool -> Bool
prop_evalNot b = BoolVal (not b) == removeJust (removeMaybe evalReturn)
                 where evalReturn = eval testMap (Not (Bool b))

prop_evalSin :: Float -> Bool
prop_evalSin f = FloatVal (sin f) == removeJust (removeMaybe evalReturn)
                 where evalReturn = eval testMap (Sin (FVal f))

prop_evalSinInt :: Int -> Bool
prop_evalSinInt f = FloatVal (sin a) == removeJust (removeMaybe evalReturn)
                    where evalReturn = eval testMap (Sin (Val f))
                          a = (fromIntegral f)::Float

prop_evalCos :: Float -> Bool
prop_evalCos f = FloatVal (cos f) == removeJust (removeMaybe evalReturn)
                 where evalReturn = eval testMap (Cos (FVal f))

prop_evalCosInt :: Int -> Bool
prop_evalCosInt f = FloatVal (cos a) == removeJust (removeMaybe evalReturn)
                    where evalReturn = eval testMap (Cos (Val f))
                          a = (fromIntegral f)::Float

prop_evalTan :: Float -> Bool
prop_evalTan f = FloatVal (tan f) == removeJust (removeMaybe evalReturn)
                 where evalReturn = eval testMap (Tan (FVal f))

prop_evalTanInt :: Int -> Bool
prop_evalTanInt f = FloatVal (tan a) == removeJust (removeMaybe evalReturn)
                    where evalReturn = eval testMap (Tan (Val f))
                          a = (fromIntegral f)::Float

prop_evalArcsin :: Float -> Bool
prop_evalArcsin f | f <= 1 && f >= -1 = FloatVal (asin f) == removeJust (removeMaybe evalReturn)
                  | otherwise         = True --throws an error
                                        where evalReturn = eval testMap (Arcsin (FVal f))

prop_evalArcsinInt :: Int -> Bool
prop_evalArcsinInt f | f <= 1 && f >= -1 = FloatVal (asin a) == removeJust (removeMaybe evalReturn)
                     | otherwise         = True --throws an error
                                           where evalReturn = eval testMap (Arcsin (Val f))
                                                 a = (fromIntegral f)::Float

prop_evalArccos :: Float -> Bool
prop_evalArccos f | f <= 1 && f >= -1 = FloatVal (acos f) == removeJust (removeMaybe evalReturn)
                  | otherwise         = True --throws an error
                                        where evalReturn = eval testMap (Arccos (FVal f))

prop_evalArccosInt :: Int -> Bool
prop_evalArccosInt f | f <= 1 && f >= -1 = FloatVal (acos a) == removeJust (removeMaybe evalReturn)
                     | otherwise         = True --throws an error
                                           where evalReturn = eval testMap (Arccos (Val f))
                                                 a = (fromIntegral f)::Float

prop_evalArctan :: Float -> Bool
prop_evalArctan f = FloatVal (atan f) == removeJust (removeMaybe evalReturn)
                    where evalReturn = eval testMap (Arctan (FVal f))

prop_evalArctanInt :: Int -> Bool
prop_evalArctanInt f = FloatVal (atan a) == removeJust (removeMaybe evalReturn)
                       where evalReturn = eval testMap (Arctan (Val f))
                             a = (fromIntegral f)::Float

prop_evalSinh :: Float -> Bool
prop_evalSinh f = FloatVal (sinh f) == removeJust (removeMaybe evalReturn)
                  where evalReturn = eval testMap (Sinh (FVal f))

prop_evalSinhInt :: Int -> Bool
prop_evalSinhInt f = FloatVal (sinh a) == removeJust (removeMaybe evalReturn)
                     where evalReturn = eval testMap (Sinh (Val f))
                           a = (fromIntegral f)::Float

prop_evalCosh :: Float -> Bool
prop_evalCosh f = FloatVal (cosh f) == removeJust (removeMaybe evalReturn)
                  where evalReturn = eval testMap (Cosh (FVal f))

prop_evalCoshInt :: Int -> Bool
prop_evalCoshInt f = FloatVal (cosh a) == removeJust (removeMaybe evalReturn)
                     where evalReturn = eval testMap (Cosh (Val f))
                           a = (fromIntegral f)::Float

prop_evalTanh :: Float -> Bool
prop_evalTanh f = FloatVal (tanh f) == removeJust (removeMaybe evalReturn)
                  where evalReturn = eval testMap (Tanh (FVal f))

prop_evalTanhInt :: Int -> Bool
prop_evalTanhInt f = FloatVal (tanh a) == removeJust (removeMaybe evalReturn)
                     where evalReturn = eval testMap (Tanh (Val f))
                           a = (fromIntegral f)::Float

prop_evalArcsinh :: Float -> Bool
prop_evalArcsinh f = FloatVal (asinh f) == removeJust (removeMaybe evalReturn)
                     where evalReturn = eval testMap (Arcsinh (FVal f))

prop_evalArcsinhInt :: Int -> Bool
prop_evalArcsinhInt f = FloatVal (asinh a) == removeJust (removeMaybe evalReturn)
                        where evalReturn = eval testMap (Arcsinh (Val f))
                              a = (fromIntegral f)::Float

prop_evalArccosh :: Float -> Bool
prop_evalArccosh f | f >= 1 && f <= -1 = FloatVal (acosh f) == removeJust (removeMaybe evalReturn)
                   | otherwise         = True --throws an error
                                         where evalReturn = eval testMap (Arccosh (FVal f))

prop_evalArccoshInt :: Int -> Bool
prop_evalArccoshInt f | f >= 1 && f <= -1 = FloatVal (acosh a) == removeJust (removeMaybe evalReturn)
                      | otherwise         = True --throws an error
                                            where evalReturn = eval testMap (Arccosh (Val f))
                                                  a = (fromIntegral f)::Float

prop_evalArctanh :: Float -> Bool
prop_evalArctanh f | f < 1 && f > -1   = FloatVal (acos f) == removeJust (removeMaybe evalReturn)
                   | otherwise         = True --throws an error
                                         where evalReturn = eval testMap (Arccos (FVal f))

prop_evalArctanhInt :: Int -> Bool
prop_evalArctanhInt f | f < 1 && f > -1   = FloatVal (acos a) == removeJust (removeMaybe evalReturn)
                      | otherwise         = True --throws an error
                                            where evalReturn = eval testMap (Arccos (Val f))
                                                  a = (fromIntegral f)::Float

prop_evalAbsInt :: Int -> Bool
prop_evalAbsInt a = IntVal (abs a) == removeJust (removeMaybe evalReturn)
                    where evalReturn = eval testMap (Abs (Val a))

prop_evalAbsFloat :: Float -> Bool
prop_evalAbsFloat a = FloatVal (abs a) == removeJust (removeMaybe evalReturn)
                      where evalReturn = eval testMap (Abs (FVal a))



--Run The Tests
return []
runTests = $quickCheckAll