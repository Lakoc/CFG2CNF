import GrammarTypes (ContextFreeGrammar)
import CFGParser (parseCFG)
import ValidateCFG (validateCFG)
import CFG2CNF (convertToCNF)
import SimplifyCFG (simplifyCFG)
import Data.List
import Errors (CustomError (..), codeToNumber)
import System.Environment (getArgs)
import System.Exit

-- Program options
dumpCFG :: ContextFreeGrammar -> IO ()
dumpCFG cfg = do
  putStr (show cfg)

removeSimpleRules :: ContextFreeGrammar -> IO ()
removeSimpleRules cfg = do
    dumpCFG $ simplifyCFG cfg

dumpCNF :: ContextFreeGrammar -> IO ()
dumpCNF cfg = do
    putStr (show $ convertToCNF ( simplifyCFG cfg))

-- Print error message to stdout and exit with non-zero code
dumpError :: CustomError -> String -> IO ()
dumpError err message = do
  putStrLn ("An Error has occured!\nError code: " ++ show (codeToNumber err) ++ "\nError message: " ++ show err ++ message)
  exitWith (ExitFailure (codeToNumber err))

-- Check program option
procOptions :: Either CustomError (String, IO String) -> Either CustomError (ContextFreeGrammar -> IO (), IO String)
procOptions val = case val of
  Left err -> Left err
  Right (arg, input) -> case arg of
    "-i" -> Right (dumpCFG, input)
    "-1" -> Right (removeSimpleRules, input)
    "-2" -> Right (dumpCNF, input)
    _ -> Left UnknownArgument

-- Checks correct number of program arguments
procArgs :: [String] -> Either CustomError (String, IO String)
procArgs [] = Left NoArgument
procArgs [arg] = Right (arg, getContents)
procArgs [arg, file] = Right (arg, readFile file)
procArgs _ = Left MoreArguments

-- Main Function
main :: IO ()
main = do
  -- Load arguments
  args <- getArgs

  -- Process arguments
  case procOptions $ procArgs args of
    Left err -> dumpError err ""
    Right validArgs -> do
      -- Parse input
      inputStr <- snd validArgs
      case parseCFG inputStr of
        Left err -> dumpError ParseError (intercalate "" $ lines (show err))
        -- Process action specified in argument
        Right gram -> case validateCFG gram of
              Left err -> dumpError err ""
              Right validCFG -> fst validArgs validCFG
