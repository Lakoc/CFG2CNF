module Errors where

data CustomError = NoArgument | MoreArguments | UnknownArgument | InvalidCFG | ParseError

instance Show CustomError where
  show NoArgument = "Invalid number of arguments. Provide atleast one argument."
  show MoreArguments = "Too many arguments."
  show UnknownArgument = "Unknown program arguments."
  show InvalidCFG = "Invalid context free grammar."
  show ParseError = "Parsing failed."

codeToNumber :: CustomError -> Int
codeToNumber enum = case enum of
  NoArgument -> 1
  MoreArguments -> 2
  UnknownArgument -> 3
  InvalidCFG -> 4
  ParseError -> 5