module REPL where

import Expr
import Parsing
import Data.Either
import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.History
import Data.Map
import qualified Data.Map as Map
import Data.List

--Name of the function and num of arguments
data FuncSig = FuncID Name Int
  deriving (Eq, Ord)

--Type returned arguments commands
data FuncBody = FuncData [Expr] [Command]

--initial state set to two empty maps
initState :: (Map Name Lit, Map FuncSig FuncBody)
initState = (Map.empty, Map.empty)

-- Given a variable name and a value, if the variable name is in the map
-- update the entry, otherwise insert the name and the value into the map
updateVars :: Name -> Lit -> Map Name Lit -> Map Name Lit
updateVars n i vars =if Map.member n vars then
                        Map.adjust (const i) n vars
                     else
                          Map.insert n i vars

process :: (Map Name Lit, Map FuncSig FuncBody) -> Command -> IO (Map Name Lit, Map FuncSig FuncBody)
--process Set commands
process st (Set var e)
     = do if isLeft (eval (fst st) e)
               --handle error
               then do putStrLn $ litToString $ removeJust (removeMaybe (eval (fst st) e))
                       return st
          else do
               let lit = removeJust (removeMaybe (eval (fst st) e))
               let st' = (updateVars var lit (fst st), snd st)
               return st'

--process Print commands
process st (Print e)
     = do putStrLn $ litToString (removeJust $ removeMaybe (eval (fst st) e))
          return st

--process Repeat commands
process st (Repeat n commands)
       -- if it is repeating less than 1 times just return
     | n < 1     = return st
     -- if it is repeating once pass the commands to the processMultipleCommands method to
     -- process them all
     | n == 1    = do processMultipleCommands st commands
     -- if it is repeating more than once pass the commands into the processMultiple commands method
     -- and process another Repeat command with the number of repeats decremented by 1, and the state
     -- returned from the processMultipleCommands method
     | otherwise = do st' <- processMultipleCommands st commands
                      process st' (Repeat (n-1) commands)

--process While commands
process st (While c body)
       -- if the boolean (c) is false then just return
     | removeJust (removeMaybe (eval (fst st) c)) == BoolVal False = return st
       -- if the boolean (c) is true then process the commands and call teh process method
       -- on another while command
     | otherwise = do st' <- processMultipleCommands st body
                      process st' (While c body)

--process function definitions
process st (Def name args body)
       --if the function is already defined replace it with the new definition
     = if Map.member (FuncID name (length args)) (snd st) then
          return ((fst st), Map.adjust (const (FuncData args body)) (FuncID name (length args)) (snd st))
       else
            --otherwise insert the function into the map
          do let st' = Map.insert (FuncID name (length args)) (FuncData args body) (snd st)
             let st'' = (fst st, st')
             return st''

--process function calls
process st (Call name args)
       -- if the function is in the map, get the funcBody
     = if Map.member (FuncID name (length args)) (snd st) then do let func = removeJust $ Map.lookup (FuncID name (length args)) (snd st)
                                                                  --get a map of Map Name Lit that will act as our variable list when processing the commands
                                                                  -- inside the function, this maps the variable names in the definition to the values passed in
                                                                  let tempMap = toMap (fst st) (merge (funcBodyArgList func) args) Map.empty
                                                                  --process multiple commands with the map of the variables accessible in the function
                                                                  -- being passed in as the map
                                                                  processMultipleCommands (tempMap, (snd st)) (funcBodyCommandList func)
                                                                  --return the original state as functions cause no changes to any external variables
                                                                  return st
       -- if the function is not in the map, print an error
       else do putStrLn "Function not defined"
               return st

--process If calls
process st (If cond body)
       -- if the condition is false return without doing anything
     | (removeJust $ removeMaybe $ eval (fst st) cond) == BoolVal False = return st
       -- otherwise process all the commands
     | otherwise                                                        = processMultipleCommands st body

--used to process multiple commands in one go
processMultipleCommands :: (Map Name Lit, Map FuncSig FuncBody) -> [Command] -> IO (Map Name Lit, Map FuncSig FuncBody)
                                      -- if the length of the command list is less than 1 return state and do nothing
processMultipleCommands st commands | length commands < 1  = return st
                                      -- if the length of the command list is 1 then process the command
                                    | length commands == 1 = do st' <- process st (head commands)
                                                                return st'
                                     -- otherwise process the first comand and call this function again on the tail of the command list
                                    | otherwise            = do st' <- process st (head commands)
                                                                st'' <- processMultipleCommands st' (tail commands)
                                                                return st''

-- get the list of argument names from FuncBody
funcBodyArgList :: FuncBody -> [Expr]
funcBodyArgList (FuncData a c) = a

--get the list of commands from FuncBody
funcBodyCommandList :: FuncBody -> [Command]
funcBodyCommandList (FuncData a c) = c

--gets the variable name from an Expr
--returns an error if the Expr is not a Var
exprToName :: Expr -> Name
exprToName (Var n) = n
exprToName _       = error "incorrect variable allocation"

--merge 2 lists of expressions into a list of tuples containing the variable name and its related expression
merge :: [Expr] -> [Expr] -> [(Name, Expr)]
merge [] _ = []
merge _ [] = []
merge [a] [b] = [(exprToName a, b)]
merge a b = [(exprToName (head a), head b)] ++ (merge (tail a) (tail b))

--takes in a list of tuples with names and their related expressions and puts each entry in the list into a Map and returns it
-- this is the map used to create a virtual enviroment for the functions to run in
toMap :: Map Name Lit -> [(Name, Expr)] -> Map Name Lit -> Map Name Lit
toMap st [] map = map
toMap st vars map = toMap st (tail vars) (Data.Map.insert (fst (head vars)) (removeJust $ removeMaybe (eval st (snd (head vars)))) map)

-- prints out Str "variable_name" or Val number rather than "variable_name" or number
litToString :: Lit -> String
litToString (StrVal a) = a
litToString (IntVal a) = litToString (StrVal (show a))
litToString (BoolVal a) = litToString (StrVal (show a))
litToString (FloatVal a) = litToString (StrVal (show a))


--This is the function that loops continuously, until the program terminates
repl :: InputT (StateT (Map Name Lit, Map FuncSig FuncBody) IO) ()
repl = do maybeInput <- getInputLine "> "
          --if the user input is Nothing or quit return as this terminates the program
          case maybeInput of
               Nothing     -> return ()
               Just "quit" -> return ()
               --tab completion leaves a space after the completed word
               Just "quit "-> return ()
               --prints out a help sheet to the terminal
               Just "help "-> do outputStrLn helpString
                                 repl
               Just "help" -> do outputStrLn helpString
                                 repl
               --any other input- get the current state stored by the StateT monad
               Just inp    -> do st <- lift get
                                 -- parse the input using the pCommand2 parser
                                 case parse pCommand2 inp of
                                      --if a command is returned process it
                                      [(cmd, "")] -> do st' <- liftIO $ process st cmd
                                                        --store the state returned by processing the command as the StateT
                                                        lift $ put st'
                                                        --restart this function
                                                        repl
                                     -- if the input has not been parsed correctly print out parse error
                                      _           -> do outputStrLn "Parse Error"
                                                        repl

--string that shows the user how to use
helpString :: String
helpString = "-- Types Supported --\n\nInteger 124\nFloat -1.34e5\nString \"string\"\nBoolean True/False\n\n\n-- Mathematical Operators --\n\nAdd + (also used to concatenate two strings)\nSubtract -\nMultiply *\nDivide /\nRaise to the Power ^\nModulus %\nFactorial !\nAbsolute Value |value|\n\n\n-- Trigonometric Functions --\n\nsin\ncos\ntan\nsinh\ncosh\ntanh\nasin\nacos\natan\nasinh\nacosh\natanh\n\n\n-- Comparisons --\nCan compare types Float and Integer against each other\n\nEquals ==\nLess Than <\nGreater Than >\nLess Than Or Equals <=\nGreater Than or Equals >=\nNot Equals ~=\n\n\n-- Boolean Operators --\n\nAnd &&\nOr ||\nNot ~\nImplies ->\n\n\n-- Recursion --\n\nRepeat x times\n# x {commands}\n\nWhile x is true\n?x? <<commands>>\n\n\n-- Decisions --\n\nIf x do something\n?x {commands}\n\n\n-- Function Definition/Calling --\nBe aware that there are no global variables\n\n:function_name(variable, names) = {commands}\n\n:function_name(values, passed_in)\n\n\n-- Conversion Functions --\n\nToInt\n|value_to_be_converted_to_int\n\nToString\n_value_to_be_converted_to_string"


--used to define the haskelline settings
--leaves completion to be sorted out by the completion function
--adds to the history automatically
--doesnt store the history in a text file
haskelineSettings :: Settings (StateT (Map Name Lit, Map FuncSig FuncBody) IO)
haskelineSettings = Settings {complete = completion,
                              autoAddHistory = True,
                              historyFile = Nothing}

--uses the haskelline function completeWord and the tabCompletion function to auto-complete words when tab is pressed
-- if tabCompletion returns one option when tab is pressed it will auto-complete
-- otherwise it will print out the options
completion :: CompletionFunc (StateT (Map Name Lit, Map FuncSig FuncBody) IO)
completion = completeWord Nothing " \t" tabCompletion

-- gets the state from StateT, then filters all the variable names that start with the letters typed, then converts these to Completions and returns them
tabCompletion :: String -> StateT (Map Name Lit, Map FuncSig FuncBody) IO [Completion]
tabCompletion str = do st <- get
                       pure $ fmap (\s -> Completion s s True) $ Prelude.filter (str `isPrefixOf`) (keys (fst st) ++ ["quit", "help"])






--reads in the lines of a file into a list of commands
replForFiles :: (Map Name Lit, Map FuncSig FuncBody) -> String -> IO()
replForFiles st filepath = do commands <- getLinesFromFile filepath
                              --call replMultipleCommands on the commands
                              runStateT (replMultipleCommands commands) st
                              return ()



replMultipleCommands :: [String] -> StateT (Map Name Lit, Map FuncSig FuncBody) IO ()
-- if the commands list passed in is empty print out Done and return
replMultipleCommands [] = do liftIO $ putStrLn "Done"
                             return ()
                                    -- parse the first line in the list
replMultipleCommands commands = case parse pCommand2 (head commands) of
                                                       --get the current state
                                         [(cmd, "")] -> do st <- get
                                                       -- process the command parsed from the line
                                                           st' <- liftIO $ process st cmd
                                                           --store the current state
                                                           put st'
                                                           --call thsi function again on the tail of the commands list
                                                           replMultipleCommands (tail commands)
                                        -- if one of the lines in the file cannot be parsed, terminate the running
                                         _ -> do liftIO $ putStrLn "Error Parsing File"
                                                 return ()

--get the lines from the file in a list of strings
getLinesFromFile :: String -> IO[String]
getLinesFromFile filepath = do fileContent <- readFile filepath
                               let fileLines = lines fileContent
                               return fileLines