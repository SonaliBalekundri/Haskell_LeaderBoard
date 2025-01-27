{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Main
-- Description : Main entry point for the leaderboard simulation program.
-- Maintainer  : YourEmail@example.com
-- Stability   : experimental
-- Portability : portable
--
-- This program simulates a leaderboard system with multiple clients generating random scores over
-- several rounds. The scores are aggregated, and the final leaderboard is written to an HTML file.
-- It demonstrates multithreading using Haskell's `forkIO` and shared state using `MVar`.

-- | The main entry point of the program.
-- 
-- The program initializes the server and client threads, manages the leaderboard state,
-- and writes the final leaderboard to an HTML file after aggregating scores over multiple rounds.

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM_)
import System.Random (randomRIO)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Server (processRequest)
import Client (simulateClient)
import Types
import LeaderboardWriter (writeLeaderboard)

main :: IO ()
main = do
    -- Creating shared `MVar`s for request queue and leaderboard.
    requestQueueMVar <- newMVar [] 
    leaderboardMVar <- newMVar [] 

    -- Declaring number of clients and rounds.
    let numClients = 10
    let numRounds = 10

    -- Initiating the server thread to process client requests.
    forkIO $ processRequest requestQueueMVar leaderboardMVar

    -- Initiating client threads to simulate multiple clients.
    forM_ [1..numClients] $ \clientId -> 
        forkIO $ simulateClient clientId requestQueueMVar

    -- Collecting scores for all rounds.
    allRoundScores <- mapM (\roundNumber -> do
        -- Simulating a delay to represent the time between rounds.
        threadDelay (numClients * 100000) -- 0.1 seconds per client

        -- Generating random scores for all clients.
        newScores <- generateScores numClients

        -- Updating the leaderboard with new scores.
        modifyMVar_ leaderboardMVar $ \_ -> return newScores

        -- Print the current leaderboard.
        putStrLn $ "Scores for round " ++ show roundNumber ++ ": " ++ show newScores
        return (roundNumber, newScores)
        ) [1..numRounds]

    -- Aggregating scores across all rounds to find top winners.
    let aggregatedScores = aggregateScores allRoundScores

    -- Writing all-round scores and top winners to an HTML file.
    writeLeaderboard allRoundScores aggregatedScores
    putStrLn "Leaderboard written to a single HTML file."

    -- Allowing time for all threads to complete (buffer delay).
    threadDelay (2 * 1000000)
    putStrLn "Process completed! Check the HTML file for leaderboards."

-- | Generates random scores for a given number of clients.
-- Each client is assigned a random score between 50 and 150, and the scores are sorted in descending order by score.
generateScores :: Int -> IO [(Int, Int)]
generateScores numClients = do
    -- Generate a list of random scores between 50 and 150 for each client.
    scores <- mapM (\clientId -> do
        score <- randomRIO (50, 150) -- Random score between 50 and 150.
        return (clientId, score)) [1..numClients]
    -- Sort the scores in descending order by score.
    return $ reverse $ sortBy (\(_, score1) (_, score2) -> compare score1 score2) scores

-- | Aggregates scores from all rounds.
-- Combines the scores of all clients across all rounds, sums their scores, and ranks them in descending order of total scores.
aggregateScores :: [(Int, [(Int, Int)])] -> [(Int, Int)]
aggregateScores allRoundScores =
    let flatScores = concatMap snd allRoundScores
        groupedScores = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) flatScores
    in reverse $ sortBy (\(_, score1) (_, score2) -> compare score1 score2) $
       map (\group -> (fst (head group), sum (map snd group))) groupedScores
