module Main where

import CFGData (ContextFreeGrammar)
import CFGParser (parseCFG)
import Errors (CustomError (..), codeToNumber)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import System.Environment (getArgs)
import System.IO (stdout)

-- dumpBKG :: ContextFreeGrammar -> IO ()
-- dumpBKG bkg = do
--     putStrLn "Dumping bkg."

-- removeSimpleRules :: ContextFreeGrammar -> IO ()
-- removeSimpleRules bkg = do
--     putStrLn "Removing simple rules."

-- dumpCNF :: ContextFreeGrammar -> IO ()
-- dumpCNF bkg = do
--     putStrLn "Dumping cnf."

-- -- On success print the output
-- printOutput:: IO String -> IO ()
-- printOutput output = do
--     val <- output
--     putStrLn val

parseInputIO :: IO String -> ContextFreeGrammar
parseInputIO = parseCFG

-- Process program runtime option
procOptions :: String -> IO String -> Either CustomError ContextFreeGrammar
procOptions option input = case option of
  "-i" -> Right $parseCFG input
  "-1" -> Right $parseCFG input
  "-2" -> Right $parseCFG input
  _ -> Left UnknownArgument

-- Checks correct number of program arguments
procArgs :: [String] -> Either CustomError ContextFreeGrammar
procArgs [] = Left NoArgument
procArgs [arg] = procOptions arg getContents
procArgs [arg, file] = procOptions arg $ readFile file
procArgs _ = Left MoreArguments

-- Main Function
main :: IO ()
main = do
  -- Disable buffering
  hSetBuffering stdout NoBuffering

  -- Load arguments
  args <- getArgs

  -- Process program core functionality
  case procArgs args of
    Left err -> putStrLn ("An Error has occured!\nError code: " ++ show (codeToNumber err) ++ "\nError message: " ++ show err)
    Right msg -> putStr (show msg)
