{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Client
-- Description : Simulates client behavior for submitting scores in a leaderboard system.
-- Maintainer  : YourEmail@example.com
-- Stability   : experimental
-- Portability : portable
--
-- This module defines functions to simulate client behavior in the leaderboard system. 
-- Each client submits a score after a random delay, which is added to a shared request queue.

module Client where

import Types
import Control.Concurrent (MVar, threadDelay, modifyMVar_)
import System.Random (randomRIO)
import Data.Time (getCurrentTime)

-- | Simulates the behavior of a client.
-- Each client waits for a random amount of time (between 1 to 5 seconds) before generating a score submission request. The request is then sent to a shared request queue.
simulateClient :: Int -> MVar RequestQue -> IO ()
simulateClient clientId reqQueue = do
    -- Wait for a random delay (1-5 seconds) to simulate client activity.
    threadDelay =<< randomRIO (1000000, 5000000)

    -- Generate a timestamp for the request.
    timestamp <- getCurrentTime

    -- Create a sample request with the client's ID and a fixed score.
    let req = SubmitScore timestamp clientId 1 150 -- Example request
    
    -- Add the request to the shared queue.
    sendRequest req reqQueue

-- | Adds a client request to the shared request queue.
-- This function modifies the `MVar`-protected request queue by appending the new request.
sendRequest :: Request -> MVar RequestQue -> IO ()
sendRequest request reqQueue = 
    modifyMVar_ reqQueue $ \queue ->
        -- Append the request to the end of the queue.
        return (queue ++ [request])
