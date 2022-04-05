module Main where

import Parsing
import Expr
import REPL

import Control.Monad.State

import System.Environment
import System.Console.Haskeline

import Data.Map 
import qualified Data.Map as Map

main :: IO ()
main = do putStrLn "Do you want to read code from a file (y/n)"
          input <- getLine
          --should add check for one valid filepath argument
          case input of
            "y" -> do putStrLn "Enter the filepath for the desired file"
                      filepath <- getLine
                      replForFiles initLState filepath
            "n"  -> runRepl initLState
            _     -> do putStrLn "Please enter either Yes or No"
                        return ()
          --runRepl initLState
--main = runStateT repl initLState

runRepl :: (Map Name Lit, Map FuncSig FuncBody) -> IO ()
runRepl st = --do runInputT defaultSettings (runStateT repl initLState)
             do runStateT (runInputT haskelineSettings repl) initLState
                return ()

                