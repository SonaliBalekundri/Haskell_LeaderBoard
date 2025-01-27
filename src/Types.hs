{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Types
-- Description : Defines data types for requests, responses, and leaderboard management.
-- Maintainer  : YourEmail@example.com
-- Stability   : experimental
-- Portability : portable
--
-- The `Types` module contains the core data structures used in the application. 
-- It defines the request and response formats exchanged between clients and the server,
-- as well as types for the request queue and leaderboard.

module Types where

import Data.Time (UTCTime)  -- Using Data.Time (UTCTime) for timestamps.

-- | Represents a request submitted by a client to the server.
data Request = SubmitScore {
    reqtimestamp :: UTCTime,  -- ^ The timestamp when the request is made.
    clientID     :: Int,      -- ^ The unique ID of the client submitting the score.
    roundNumber  :: Int,      -- ^ The round number associated with the request.
    score        :: Int       -- ^ The score submitted by the client for the round.
} deriving (Show)

-- | Represents a response sent by the server to the client.
data Response = LeaderBoardResponse {
    restimestamp :: UTCTime,       -- ^ The timestamp when the response is generated.
    leaderBoard  :: [(Int, Int)],  -- ^ The leaderboard data as a list of tuples, where each tuple contains:
                                   -- * `clientID` - The unique ID of a client.
                                   -- * `totalScore` - The total score of the client across rounds.
    clientRank   :: Int            -- ^ The rank of the client in the leaderboard.
} deriving (Show)

-- | Represents the request queue as a list of `Request`s.
type RequestQue = [Request]

-- | Represents the leaderboard as a list of entries, where each entry contains:
type Leaderboard = [(Int, Int)]

