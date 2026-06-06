{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.MockEnv (
  MockEnv (..),
  MockBitwarden (..),
  runMockBitwarden,
  defaultMockEnv,
  mockNow,
  withUnlockResult,
  withListItemsResult,
  withGetPasswordResult,
  withSyncResult,
)
where

import Control.Monad.Time
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Instances.Time ()

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden

runMockBitwarden :: MockEnv -> MockBitwarden a -> a
runMockBitwarden mockEnv (MockBitwarden run) = run mockEnv

newtype MockBitwarden a = MockBitwarden
  { runMockBitwardenInternal :: MockEnv -> a
  }

instance Functor MockBitwarden where
  fmap f (MockBitwarden run) = MockBitwarden (f . run)

instance Applicative MockBitwarden where
  pure value = MockBitwarden (\_ -> value)
  MockBitwarden apply <*> MockBitwarden run =
    MockBitwarden (\mockEnv -> apply mockEnv (run mockEnv))

instance Monad MockBitwarden where
  MockBitwarden run >>= f =
    MockBitwarden $ \mockEnv ->
      let MockBitwarden next = f (run mockEnv)
       in next mockEnv

instance Bitwarden.Bitwarden MockBitwarden where
  unlock _ _ = MockBitwarden unlockResult
  listItems _ = MockBitwarden listItemsResult
  sync _ = MockBitwarden syncResult
  getPassword _ _ = MockBitwarden getPasswordResult
  lock _ = pure Bitwarden.LockSucceeded

instance MonadTime MockBitwarden where
  currentTime = MockBitwarden mockCurrentTime
  monotonicTime = pure 0

data MockEnv = MockEnv
  { unlockResult :: Either Agent.UnlockError Agent.SessionKey
  , listItemsResult :: Either Agent.ListItemsError [Agent.ItemSummary]
  , syncResult :: Either Bitwarden.SyncError ()
  , getPasswordResult :: Either Bitwarden.GetPasswordError Agent.PasswordValue
  , mockCurrentTime :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance Arbitrary MockEnv where
  arbitrary =
    MockEnv <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure mockNow
  shrink = genericShrink

mockNow :: UTCTime
mockNow = read "2026-05-20 12:00:05 UTC"

defaultMockEnv :: MockEnv
defaultMockEnv =
  MockEnv
    { unlockResult = Left Agent.UnlockUnavailable
    , listItemsResult = Left (Agent.ListItemsFailed "bw list items failed")
    , syncResult = Left Bitwarden.SyncUnavailable
    , getPasswordResult = Left Bitwarden.GetPasswordUnavailable
    , mockCurrentTime = mockNow
    }

withUnlockResult ::
  Either Agent.UnlockError Agent.SessionKey ->
  MockEnv ->
  MockEnv
withUnlockResult result mockEnv = mockEnv{unlockResult = result}

withListItemsResult ::
  Either Agent.ListItemsError [Agent.ItemSummary] ->
  MockEnv ->
  MockEnv
withListItemsResult result mockEnv = mockEnv{listItemsResult = result}

withGetPasswordResult ::
  Either Bitwarden.GetPasswordError Agent.PasswordValue ->
  MockEnv ->
  MockEnv
withGetPasswordResult result mockEnv = mockEnv{getPasswordResult = result}

withSyncResult :: Either Bitwarden.SyncError () -> MockEnv -> MockEnv
withSyncResult result mockEnv = mockEnv{syncResult = result}
