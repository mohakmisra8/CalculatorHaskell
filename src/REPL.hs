module REPL (
     FuncSig(FuncID),
     FuncBody(FuncData),
     replForFiles,
     initLState,
     haskelineSettings,
     repl,
     LState
) where

import Expr
import Parsing
import Data.Either
import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.History
import Data.Map
import qualified Data.Map as Map
import Data.List

type LState = (Map Name Lit, Map FuncSig FuncBody)

data FuncSig = FuncID Name [Expr]
  deriving (Eq, Ord)
 --deriving instance Ord k => Ord (FuncSig)
data FuncBody = FuncData Type [Command]

initLState :: (Map Name Lit, Map FuncSig FuncBody)
initLState = (Map.empty, Map.empty)

funcList :: Map FuncSig FuncBody
funcList = Map.empty

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
--Should be working -Ewan
--changed with maps by Mohak
updateVars :: Name -> Lit -> Map Name Lit -> Map Name Lit
updateVars n i vars =if Map.member n vars then
                        Map.adjust (const i) n vars
                     else
                          Map.insert n i vars

-- Return a new set of variables with the given name removed
--dropVar :: Name -> [(Name, Lit)] -> [(Name, Lit)]
--dropVar n = filter (\x -> fst x /= n)

process :: (Map Name Lit, Map FuncSig FuncBody) -> Command -> IO (Map Name Lit, Map FuncSig FuncBody)
process st (Set var e)
     = do putStrLn $ show $ eval (fst st) e
          if isLeft (eval (fst st) e)
               --handle error
               then do putStrLn "handle this error"
                       return st
          else do
               let lit = removeJust (removeMaybe (eval (fst st) e))
               let st' = (updateVars var lit (fst st), snd st)
               return st'
          -- we need to process the expression here before adding the result to the state

          -- st' should include the variable set to the result of evaluating e

process st (Print e)
-- prints out Str "variable_name" or Val number rather than "variable_name" or number
     = do putStrLn $ show $ eval (fst st) e
          putStrLn $ litToString (removeJust $ removeMaybe (eval (fst st) e))

          -- Print the result of evaluation
          return st

process st (Repeat n commands)
     | n < 1     = return st
     | n == 1    = do st' <- processMultipleCommands st commands
                      return st'
     | otherwise = do st' <- processMultipleCommands st commands
                      st'' <- process st' (Repeat (n-1) commands)
                      return st''

process st (While c body)
     | removeJust (removeMaybe (eval (fst st) c)) == BoolVal False = return st
     | otherwise = do st' <- processMultipleCommands st body
                      st'' <- process st' (While c body)
                      return st''

process st (Def ret_type name args body)
     = if Map.member (FuncID name args) (snd st) then
          error "Duplicate function definition attempted"
       else
          do let st' = Map.insert (FuncID name args) (FuncData ret_type body) (snd st)
             let st'' = (fst st, st')
             return st''


     {-| length body == 1 = do st' <- liftIO $ (process st (body!!0))
                             return st'
     | otherwise = do st' <- liftIO $ (process st (body!!0))
                      st'' <- liftIO $ (process st' (While c (Data.List.drop 1 body)))
                      return st''-}

processMultipleCommands :: (Map Name Lit, Map FuncSig FuncBody) -> [Command] -> IO (Map Name Lit, Map FuncSig FuncBody)
processMultipleCommands st commands | length commands <= 1 = do st' <- process st (head commands)
                                                                return st'
                                    | otherwise            = do st' <- process st (head commands)
                                                                st'' <- processMultipleCommands st' (tail commands)
                                                                return st''

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputT (StateT (Map Name Lit, Map FuncSig FuncBody) IO) ()
repl = do maybeInput <- getInputLine "> "
          --print out the map
          st <- lift get
          liftIO $ print $ toList (fst st)

          case maybeInput of
               Nothing     -> return ()
               Just "quit" -> return ()
               --tab completion leaves a space after the completed word
               Just "quit "-> return ()
               Just inp    -> do st <- lift get
                                 case parse pCommand2 inp of
                                      [(cmd, "")] -> do outputStrLn $ show cmd
                                                        st' <- liftIO $ process st cmd
                                                        --outputStrLn $ show cmd
                                                        lift $ put st'
                                                        repl
                                      _           -> do outputStrLn "Parse Error"
                                                        repl

--repl = do liftIO $ putStr "> "
          --inp <- liftIO getLine
          --st <- get
          --if inp == "quit" then return ()
          --else
             --input <- getLinesFromFile "filepath"
             --let inp = head input
          --case [(Set "variable" (Val 5), "")] of
               --case parse pCommand inp of
                    --[(cmd, "")] -> -- Must parse entire input
                             --do st <- liftIO $ process st cmd--st <- process st cmd
                                --put st
                                --st' <- get
                                --repl
                    --_ -> do liftIO $ putStrLn "Parse Error"
                            --repl

                          ---move all of this to safety file and test a little bit the other bit

haskelineSettings :: Settings (StateT (Map Name Lit, Map FuncSig FuncBody) IO)
--maybe change completeWord to completeWordWithPrev need to work out difference
--can save history to a file, should we??
haskelineSettings = Settings {complete = completion,
                              autoAddHistory = True,
                              historyFile = Nothing}

completion :: CompletionFunc (StateT (Map Name Lit, Map FuncSig FuncBody) IO)
completion = completeWord Nothing " \t" tabCompletion

tabCompletion :: String -> StateT (Map Name Lit, Map FuncSig FuncBody) IO [Completion]
tabCompletion str = do st <- get
                       pure $ fmap (\s -> Completion s s True) $ Prelude.filter (str `isPrefixOf`) (keys (fst st) ++ ["quit"])

--searchHistory :: String -> [Completion]
--searchHistory str = map simpleCompletion $ filter (str `isPrefixOf`) ()

--exampleList :: [String]
--exampleList = ["test", "variable", "quit"]






--file stuff
replForFiles :: (Map Name Lit, Map FuncSig FuncBody) -> String -> IO()
replForFiles st filepath = do commands <- getLinesFromFile filepath
                              runStateT (replMultipleCommands commands) st
                              return ()



replMultipleCommands :: [String] -> StateT (Map Name Lit, Map FuncSig FuncBody) IO ()
replMultipleCommands [] = do liftIO $ putStrLn "Done"
                             return ()
replMultipleCommands commands = case parse pCommand (head commands) of
                                         [(cmd, "")] -> do st <- get
                                                           st' <- liftIO $ process st cmd
                                                           put st'
                                                           replMultipleCommands (tail commands)
                                         _ -> do liftIO $ putStrLn "Error Parsing File"
                                                 return ()

--[(Command, String)]
--[(Command, "")] do the stuff 
--Parse Error

getLinesFromFile :: String -> IO[String]
getLinesFromFile filepath = do fileContent <- readFile filepath
                               let fileLines = lines fileContent
                               return fileLines