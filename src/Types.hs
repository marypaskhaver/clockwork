module Types where

type Minutes = Int

data Period = A | P deriving (Eq)

data ArgumentsError = TooFew | TooMany | InvalidCommand

instance Show ArgumentsError where
  show TooFew = "Too few arguments."
  show TooMany = "Too many arguments."
  show InvalidCommand = "Invalid command."

-- Valid commands to perform on time ranges
data Command = Sum
