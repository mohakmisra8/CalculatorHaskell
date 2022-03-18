module REPL where

import Expr
import Parsing

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

process :: LState -> Command -> IO ()
process st (Set var e)
     = do let st' = LState {vars = updateVars var (getLit e) (vars st)}
          putStrLn "Set variable"
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Print e)
-- prints out Str "variable_name" or Val number rather than "variable_name" or number
     = do putStrLn (litToString (removeJust ((eval (vars st) (ToString e)))))
          -- Print the result of evaluation
          repl st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do putStr "> "
             inp <- getLine
             --case [(Set "variable" (Str "InVariable"), "")] of
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                             process st cmd
                  _ -> do putStrLn "Parse Error"
                          repl st

--[(Command, String)]
--[(Command, "")] do the stuff 
--Parse Error