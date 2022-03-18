module REPL where

import Expr
import Parsing
import Control.Monad.State

data LState = LState { vars :: [(Name, Lit)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
--Should be working -Ewan
updateVars :: Name -> Lit -> [(Name, Lit)] -> [(Name, Lit)]
updateVars n i vars = filter (\x -> fst x /= n) vars ++ [(n,i)]

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Lit)] -> [(Name, Lit)]
dropVar n = filter (\x -> fst x /= n)

removeJust :: Maybe a -> a
removeJust (Just a) = a

process :: LState -> Command -> IO (LState)
process st (Set var e)
     = do let lit = removeJust (eval (vars st) e)
          -- we need to process the expression here before adding the result to the state
          let st' = LState {vars = updateVars var lit (vars st)}
          -- st' should include the variable set to the result of evaluating e
          return st'
process st (Print e)
-- prints out Str "variable_name" or Val number rather than "variable_name" or number
     = do putStrLn $ litToString (removeJust (eval (vars st) e))
          -- Print the result of evaluation
          return st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: StateT LState IO ()
repl = do liftIO $ putStr "> "
          inp <- liftIO getLine
          st <- get
          if inp == "quit" then return ()
          else
             --input <- getLinesFromFile "filepath"
             --let inp = head input
          --case [(Set "variable" (Val 5), "")] of
               case parse pCommand inp of
                    [(cmd, "")] -> -- Must parse entire input
                             do st <- liftIO $ process st cmd--st <- process st cmd
                                put st
                                st' <- get
                                repl
                    _ -> do liftIO $ putStrLn "Parse Error"
                            repl

                          ---move all of this to safety file and test a little bit the other bit










--file stuff
replForFiles :: LState -> String -> IO()
replForFiles st filepath = do commands <- getLinesFromFile filepath
                              runStateT (processMultipleCommands commands) st
                              return ()



processMultipleCommands :: [String] -> StateT LState IO ()
processMultipleCommands [] = do liftIO $ putStrLn "Done"
                                return ()
processMultipleCommands commands = case parse pCommand (head commands) of
                                         [(cmd, "")] -> do st <- get
                                                           st' <- liftIO $ process st cmd
                                                           put st'
                                                           processMultipleCommands (tail commands)
                                         _ -> do liftIO $ putStrLn "Error Parsing File"
                                                 return ()

--[(Command, String)]
--[(Command, "")] do the stuff 
--Parse Error

getLinesFromFile :: String -> IO[String]
getLinesFromFile filepath = do fileContent <- readFile filepath
                               let fileLines = lines fileContent
                               return fileLines