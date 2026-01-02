module Main (main) where

import Control.Monad
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import Text.Read
import Types

main :: IO ()
main = do
  putStrLn $
    "To run this program in ghci, use :run main <command> <file_path>"
      ++ "\n\nPossible commands: \n\tmin\tFind the shortest time range in the file"
      ++ "\n\tsum\tSum the time ranges in the file"
      ++ "\n\nTime ranges in the given file must be line-separated and in the formats: hh:mmA/P-hh:mmA/P\n"
      ++ "Currently, if the file at the given path contains a range in an invalid format, the program will ignore it.\n"
  -- TODO: Add a config argument to allow the user to customize how they want the program to
  -- interpret invalid lines of input. For example: Stop the entire program vs. skip the invalid line.
  arguments <- getArgs

  case parseArguments arguments of
    Left argumentError -> error (show argumentError)
    Right (command, filePath) -> do
      handle <- openFile filePath ReadMode
      contents <- hGetContents handle

      let potentialTimeRanges = lines contents
          parsedTimeRangeLengths = fmap getLengthOfTimeRange potentialTimeRanges
          definedTimeRangeLengths = catMaybes parsedTimeRangeLengths

      case command of
        Sum -> do
          let summedTimeRangeLengths = sum definedTimeRangeLengths
          putStrLn (minutesToString summedTimeRangeLengths)
        Min -> putStrLn "Not implemented yet."

      hClose handle

parseArguments :: [String] -> Either ArgumentsError (Command, FilePath)
parseArguments [] = Left TooFew
parseArguments [_] = Left TooFew
parseArguments (potentialCommand : rest) = do
  parseCommand potentialCommand >>= \command -> do
    case rest of
      [filePath] -> Right (command, filePath)
      _ -> Left TooMany
  where
    -- TODO: Implement once you define commands to perform on time ranges in files.
    parseCommand potential
      | (lowered == "sum") = Right Sum
      | (lowered == "min") = Right Min
      | otherwise = Left InvalidCommand
      where
        lowered = map toLower potential

-- The length of a given time range in minutes.
getLengthOfTimeRange :: String -> Maybe Minutes
getLengthOfTimeRange range = do
  let (potentialStartTime, potentialEndTime) = splitTwain (== '-') range

  case (parseTimeToMinutes potentialStartTime, parseTimeToMinutes potentialEndTime) of
    (Just (startMinutes, startPeriod), Just (endMinutes, endPeriod)) -> Just $ do
      case (startPeriod, endPeriod) of
        (P, A) -> endMinutes + (24 * 60) - startMinutes
        _ -> endMinutes - startMinutes
    _ -> Nothing
  where
    -- The given time of the day in minutes. For example:
    --  12:00A = 0 minutes
    --  01:00A = 60 minutes
    --  12:00P = 720 minutes
    --  01:00P = 780 minutes
    -- We're expecting time here to be in the format hh:mmA/P
    parseTimeToMinutes :: String -> Maybe (Minutes, Period)
    parseTimeToMinutes time = do
      let (potentialSeparatedNumbers, potentialPeriodString) = break (\ch -> ch == 'A' || ch == 'P') time
          mPeriod = case potentialPeriodString of
            "A" -> Just A
            "P" -> Just P
            _ -> Nothing

      guard (length potentialSeparatedNumbers == 5)

      mPeriod >>= \period -> do
        let (hourString, minuteString) = splitTwain (== ':') potentialSeparatedNumbers

        guard (length hourString == 2 && length minuteString == 2)

        case (readMaybe hourString, readMaybe minuteString) of
          (Just hour, Just minute) -> do
            if (hour < 0 || hour > 12 || minute < 0 || minute > 59)
              then Nothing
              else Just $ ((hour * 60) + (if hour < 12 && period == P then (12 * 60) else 0) + minute, period)
          (_, _) -> Nothing

-- Splits a String into a tuple where the first item is everything before the character that
-- matches the predicate and the second is everything after it.
splitTwain :: (Char -> Bool) -> String -> (String, String)
splitTwain predicate string = do
  let (beforeDelimiter, delimiterAndAfter) = break predicate string
  (beforeDelimiter, drop 1 delimiterAndAfter)

-- TODO: Add a config argument that allows the user to specify how they want time ranges displayed. For example,
-- as minutes, hours and minutes, days and hours and minutes, etc.
minutesToString :: Minutes -> String
minutesToString minutes = do
  let minutesInAnHour = 60
      hours = minutes `div` minutesInAnHour
      remainingMinutes = minutes `mod` minutesInAnHour
  "Hours: " ++ (show hours) ++ ", Minutes: " ++ (show remainingMinutes)
