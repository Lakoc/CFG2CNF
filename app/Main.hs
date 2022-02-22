module Main where

import CFGData (ContextFreeGrammar)
import CFGParser (parseCFG)
import Data.List
import Errors (CustomError (..), codeToNumber)
import System.Environment (getArgs)
import System.Exit

dumpCFG :: ContextFreeGrammar -> IO ()
dumpCFG cfg = do
  putStr (show cfg)

-- removeSimpleRules :: ContextFreeGrammar -> IO ()
-- removeSimpleRules bkg = do
--     putStrLn "Removing simple rules."

-- dumpCNF :: ContextFreeGrammar -> IO ()
-- dumpCNF bkg = do
--     putStrLn "Dumping cnf."

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
    "-1" -> Right (dumpCFG, input)
    "-2" -> Right (dumpCFG, input)
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
        Right gram -> fst validArgs gram
