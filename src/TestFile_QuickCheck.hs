module TestFile_QuickCheck where

--imports go here
import Test.QuickCheck
import Test.QuickCheck.Monadic

import REPL
import Expr

--Tests Go Here

--checks that integer values can be added to a variable
prop_testUpdateVars1 :: String -> Int -> Bool
prop_testUpdateVars1 var val = updateVars var (IntVal val) [] == returnList
                               where returnList = [(var, (IntVal val))]

--checks that string value can be added to a variable
prop_testUpdateVars2 :: String -> String -> Bool
prop_testUpdateVars2 var val = updateVars var (StrVal val) [] == returnList
                               where returnList = [(var, (StrVal val))]

--checks that integer values stored in variables can be updated
prop_testUpdateVars3 :: String -> Int -> Int -> Bool
prop_testUpdateVars3 var val1 val2 = updateVars var (IntVal val2) (updateVars var (IntVal val1) []) == returnList
                                     where returnList = [(var, (IntVal val2))]

--checks that string values stored in variables can be updated
prop_testUpdateVars4 :: String -> String -> String -> Bool
prop_testUpdateVars4 var val1 val2 = updateVars var (StrVal val2) (updateVars var (StrVal val1) []) == returnList
                                     where returnList = [(var, (StrVal val2))]

--checks that a variable storing an integer can be updated to store a string
prop_testUpdateVars5 :: String -> Int -> String -> Bool
prop_testUpdateVars5 var val1 val2 = updateVars var (StrVal val2) (updateVars var (IntVal val1) []) == returnList
                                     where returnList = [(var, (StrVal val2))]

--checks that a variable storing an integer can be updated to store a string
prop_testUpdateVars6 :: String -> String -> Int -> Bool
prop_testUpdateVars6 var val1 val2 = updateVars var (IntVal val2) (updateVars var (StrVal val1) []) == returnList
                                     where returnList = [(var, (IntVal val2))]

--Run The Tests
return []
runTests = $quickCheckAll