{-# LANGUAGE RecordWildCards #-}

module Types where

data ArgumentsError = TooFew | TooMany | InvalidCommand
instance Show ArgumentsError where 
  show TooFew = "Too few arguments."
  show TooMany = "Too many arguments."
  show InvalidCommand = "Invalid command."
