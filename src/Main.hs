module Main where

import Parsing
import Expr
import REPL

import Control.Monad.State

import System.Environment

main :: IO ()
main = do putStrLn "Do you want to read code from a file (Yes/No)"
          input <- getLine
          --should add check for one valid filepath argument
          --let x = "Yes"
          --let y = "No"
          --case y of
            --x -> do putStrLn "Enter the filepath for the desired file"
                    --filepath <- getLine
                    --replForFiles initLState filepath
            --y  -> runRepl initLState
            --_     -> do putStrLn "Please enter either Yes or No"
                        --return ()
          runRepl initLState
--main = runStateT repl initLState

runRepl :: LState -> IO ()
runRepl st = do runStateT repl initLState
                return ()