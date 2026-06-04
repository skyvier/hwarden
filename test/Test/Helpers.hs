{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time (addUTCTime)

import Test.MockEnv

import qualified Hwarden.Agent as Agent

statusResponseMatchesState :: Agent.AgentState -> Agent.Response -> Bool
statusResponseMatchesState Agent.Locked response = response == Agent.successResponse "locked"
statusResponseMatchesState (Agent.Unlocked _ _) response = response == Agent.successResponse "unlocked"

encodedResponse :: Agent.Response -> BS.ByteString
encodedResponse = LBS.toStrict . Aeson.encode

isFailure :: Agent.Response -> Bool
isFailure = Agent.responseIsFailure

encodedResponseContains :: String -> Agent.Response -> Bool
encodedResponseContains needle response =
  BS8.pack needle `BS.isInfixOf` encodedResponse response

cacheEntryRefreshedSecondsAgo :: Agent.CacheAgeSeconds -> [Agent.ItemSummary] -> Agent.CacheEntry
cacheEntryRefreshedSecondsAgo (Agent.CacheAgeSeconds ageSeconds) items =
  Agent.CacheEntry items (addUTCTime (negate (fromIntegral ageSeconds)) mockNow)
