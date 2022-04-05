module Main where

import Parsing
import Expr
import REPL

import Control.Monad.State

import System.Environment
import System.Console.Haskeline

import Data.Map 
import qualified Data.Map as Map

--Asks the user if they want to read from a file or not
main :: IO ()
main = do putStrLn "Do you want to read code from a file (y/n)"
          input <- getLine
          case input of
            --if they want to read from a file get the filepath
            "y" -> do putStrLn "Enter the filepath for the desired file"
                      filepath <- getLine
                      --run the file through a modified repl
                      replForFiles initState filepath
            --if they dont want to use a file run normally
            "n"  -> runRepl initState
            --invalid answer, prompt for valid answer
            _     -> do putStrLn "Please enter either Yes or No"
                        main

--Runs repl using InputT (haskelline) and StateT monads
runRepl :: (Map Name Lit, Map FuncSig FuncBody) -> IO ()
runRepl st = do runStateT (runInputT haskelineSettings repl) initState
                return ()

                