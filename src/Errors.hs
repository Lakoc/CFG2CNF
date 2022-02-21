module Errors where

data CustomError = NoArgument | MoreArguments | UnknownArgument

instance Show CustomError where
  show NoArgument = "Invalid number of arguments. Provide atleast one argument."
  show MoreArguments = "Too many arguments."
  show UnknownArgument = "Unknown program arguments."

codeToNumber :: CustomError -> Int
codeToNumber enum = case enum of
  NoArgument -> 1
  MoreArguments -> 2
  UnknownArgument -> 3