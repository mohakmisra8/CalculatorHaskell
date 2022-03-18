module Main where

import Parsing
import Expr
import REPL

import Control.Monad.State

main :: IO ((), LState)
main = runStateT repl initLState