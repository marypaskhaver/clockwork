module Main (main) where

import System.Environment
import System.IO
import Types

main :: IO ()
main = do
  putStrLn $ "To run this program in ghci, use :run main \"<command>\" \"<file_path>\"" ++
           "\n\nPossible commands: I'll list here when I build them."
  arguments <- getArgs

  case parseArguments arguments of
    Left argumentError -> error (show argumentError)
    Right (command, filePath) -> do
      handle <- openFile filePath ReadMode
      contents <- hGetContents handle
      putStrLn command
      putStrLn contents
      hClose handle
  
  putStrLn $ show arguments

parseArguments :: [String] -> Either ArgumentsError (String, FilePath) -- TODO: Replace String with a sum type.
parseArguments [] = Left TooFew
parseArguments [_] = Left TooFew
parseArguments (potentialCommand : rest) = do
  parseCommand potentialCommand >>= \command -> do 
    case rest of 
      [filePath] -> Right (command, filePath)
      _ -> Left TooMany
  where
    -- TODO: Implement once you define commands to perform on time ranges in files.
    parseCommand potential = Right potential