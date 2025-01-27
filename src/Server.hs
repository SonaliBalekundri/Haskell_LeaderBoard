{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Server
-- Description : Handles request processing and updates the leaderboard.
-- Maintainer  : YourEmail@example.com
-- Stability   : experimental
-- Portability : portable
--
-- The `Server` module is responsible for processing requests from clients
-- and updating the leaderboard. It continuously monitors a shared request queue,
-- processes incoming requests, and writes the updated leaderboard to an HTML file.

module Server (processRequest) where

import Control.Concurrent.MVar
import Control.Monad (forever)
import Types (Request, Leaderboard)
import LeaderboardWriter (writeLeaderboard)

-- | Continuously processes requests from the queue and updates the leaderboard.
-- The function performs the following:
-- 1. Retrieves the next request from the queue.
-- 2. Updates the leaderboard based on the request.
-- 3. Writes the updated leaderboard to an HTML file.
processRequest :: MVar [Request] -> MVar Leaderboard -> IO ()
processRequest reqQueueMVar leaderboardMVar = forever $ do
    -- Simulate processing a request.
    request <- popRequest reqQueueMVar
    case request of
        Nothing -> return () -- No requests in the queue; continue waiting.
        Just req -> do
            -- Update the leaderboard based on the request.
            modifyMVar_ leaderboardMVar $ \leaderboard -> do
                let updatedLeaderboard = updateLeaderboard req leaderboard
                let currentRoundData = (currentRound req, updatedLeaderboard) -- Wrap updated leaderboard with the current round.
                writeLeaderboard [currentRoundData] updatedLeaderboard -- Write leaderboard to HTML.
                return updatedLeaderboard

-- | Pops the next request from the queue.
popRequest :: MVar [Request] -> IO (Maybe Request)
popRequest reqQueueMVar = modifyMVar reqQueueMVar $ \queue ->
    case queue of
        [] -> return (queue, Nothing) -- Queue is empty.
        (x:xs) -> return (xs, Just x) -- Remove the first request and return it.

-- | Updates the leaderboard based on a request.
updateLeaderboard :: Request -> Leaderboard -> Leaderboard
updateLeaderboard req lb = lb -- Implement your leaderboard update logic here.

-- | Extracts the current round number from the request.
currentRound :: Request -> Int
currentRound req = 1 -- Replace with actual logic to extract the current round.
