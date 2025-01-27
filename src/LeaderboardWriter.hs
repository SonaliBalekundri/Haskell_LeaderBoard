{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : LeaderboardWriter
Description : Module for generating and managing leaderboard HTML files and logging requests/responses.
Copyright   : (c) YourName, 2025
License     : MIT
Maintainer  : your.email@example.com
Stability   : experimental
Portability : POSIX

This module provides functions to generate leaderboard HTML files and handle request/response logging.
It supports generating both tabular and card views of the leaderboard and maintains a log of operations.
-}

module LeaderboardWriter where

import System.IO
import System.Directory (getCurrentDirectory)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)

-- | Data type representing a request for logging purposes.
data Request = Request 
  { reqId :: Int            -- ^ Unique ID of the request.
  , reqContent :: String    -- ^ Content or description of the request.
  , reqTime :: UTCTime      -- ^ Timestamp of when the request was made.
  }
instance Show Request where
    show (Request id content time) =
        "\nRequest #" ++ show id ++ " at " ++ show time ++ ": " ++ content

-- | Data type representing a response for logging purposes.
data Response = Response 
  { respId :: Int            -- ^ Unique ID of the response.
  , respTime :: UTCTime      -- ^ Timestamp of when the response was sent.
  , respContent :: String    -- ^ Content or description of the response.
  }
instance Show Response where
    show (Response id time content) =
        "Response to Request #" ++ show id ++ " at " ++ show time ++ ": " ++ content

-- | Writes the leaderboard data to an HTML file and logs the operation details.
-- The leaderboard can be displayed in both tabular and card views.
writeLeaderboard :: [(Int, [(Int, Int)])] -> [(Int, Int)] -> IO ()
writeLeaderboard allRoundScores totalScoresAllRounds = do
    currentDir <- getCurrentDirectory
    let leaderboardFileName = "leaderboard.html"
    let logFileName = "request.log"
    let leaderboardFullPath = currentDir ++ "\\" ++ leaderboardFileName
    let logFilePath = currentDir ++ "\\" ++ logFileName

    -- Log starting message
    requestTime <- getCurrentTime
    let request = Request 1 "Generate Leaderboard HTML" requestTime
    appendFile logFilePath (show request ++ "\n")

    -- Write the HTML leaderboard file
    putStrLn $ "Writing leaderboard to file: " ++ leaderboardFullPath
    writeFile leaderboardFullPath (generateHtml allRoundScores totalScoresAllRounds)
    putStrLn $ "File written successfully: " ++ leaderboardFullPath

    -- Log response message
    responseTime <- getCurrentTime
    let response = Response 1 responseTime "Generated and saved"
    appendFile logFilePath (show response ++ "\n")

    -- Calculate and log time taken
    let timeTaken = diffUTCTime responseTime requestTime
    appendFile logFilePath ("Time taken: " ++ show timeTaken ++ " seconds\n")

    -- Debugging totalScoresAllRounds content
    logMessage logFilePath "Total Scores from all rounds:"
    logMessage logFilePath (show totalScoresAllRounds)

-- | Generates the HTML content for the leaderboard, including both table and card views.
generateHtml :: [(Int, [(Int, Int)])] -> [(Int, Int)] -> String
generateHtml allRoundScores totalScoresAllRounds =
    "<!DOCTYPE html>\n<html>\n<head>\n<title>Leaderboard</title>\n<style>\n" ++
    generateStyles ++
    "</style>\n</head>\n<body>\n" ++
    "<h1 style='text-align:center;'>LEADER-BOARD</h1>\n" ++
    -- Display all rounds side by side
    "<div class='rounds-container'>\n" ++
    concatMap generateRoundTable allRoundScores ++
    "</div>\n" ++
    generateCardView totalScoresAllRounds ++
    "</body>\n</html>"

-- | Generates an HTML table for a single round of the leaderboard.
generateRoundTable :: (Int, [(Int, Int)]) -> String
generateRoundTable (roundNumber, leaderboard) =
    "<div style='flex: 1; min-width: 300px;'>\n" ++
    "<h2 style='text-align:center;'>Round " ++ show roundNumber ++ "</h2>\n" ++
    "<table>\n<tr><th>Rank</th><th>Client ID</th><th>Total Score</th></tr>\n" ++
    concatMap generateRow (zip [1..] leaderboard) ++
    "</table>\n</div>\n"

-- | Generates an HTML card view for the cumulative leaderboard.
generateCardView :: [(Int, Int)] -> String
generateCardView totalScoresAllRounds =
    "<h2 style='text-align:center; color: green;'>TOTAL SCORES FROM ALL THE ROUNDS</h2>\n" ++
    "<div class='card-container'>\n" ++
    concatMap generateCard (zip [1..] totalScoresAllRounds) ++
    "</div>\n"

-- | Generates an HTML card for a single player in the leaderboard.
generateCard :: (Int, (Int, Int)) -> String
generateCard (rank, (clientId, score)) =
    "<div class='card'>\n" ++
    "<div class='avatar'></div>\n" ++
    "<div class='card-content'>\n" ++
    "<p><b>Rank:</b> " ++ show rank ++ "</p>\n" ++
    "<p><b>Client ID:</b> " ++ show clientId ++ "</p>\n" ++
    "<p><b>Score:</b> " ++ show score ++ "</p>\n" ++
    "</div>\n</div>\n"

-- | Generates a single row for an HTML table.
generateRow :: (Int, (Int, Int)) -> String
generateRow (rank, (clientId, score)) =
    "<tr><td>" ++ show rank ++ "</td><td>" ++ show clientId ++ "</td><td>" ++ show score ++ "</td></tr>\n"

-- | Generates CSS styles for the leaderboard, including table and card view styling.
generateStyles :: String
generateStyles =
    "table {border-collapse: collapse; width: 80%; margin: 10px;}\n" ++
    "th {border: 1px solid black; padding: 8px; text-align: center; background-color: #f2f2f2; color: brown;}\n" ++
    "td {border: 1px solid black; padding: 8px; text-align: center;}\n" ++
    ".rounds-container {display: flex; justify-content: space-evenly; flex-wrap: wrap;}\n" ++
    ".card-container {display: flex; flex-wrap: wrap; justify-content: center; gap: 15px;}\n" ++
    ".card {width: 200px; border: 1px solid #ccc; border-radius: 8px; padding: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); text-align: center; background-color: #f9f9f9;}\n" ++
    ".avatar {width: 80px; height: 80px; border-radius: 50%; background-color: #bbb; margin: 0 auto 10px;}\n" ++
    ".card-content {font-family: Arial, sans-serif;}\n"

-- | Appends a log message to a log file.
logMessage :: FilePath -> String -> IO ()
logMessage logFilePath message = do
    appendFile logFilePath (message ++ "\n")
