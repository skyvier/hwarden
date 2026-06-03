{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sanitization.LeakingMockEnv
  ( LeakingMockEnv (..)
  ) where

import Data.Data
import qualified Data.Text as T
import GHC.TypeLits
import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Test.MockEnv
import Test.Tasty.QuickCheck

newtype LeakingMockEnv (sessionKey :: Symbol) (password :: Symbol) = LeakingMockEnv MockEnv
  deriving newtype (Show)

instance (KnownSymbol sessionKey, KnownSymbol password) => Arbitrary (LeakingMockEnv sessionKey password) where
  arbitrary = do
    let
      sessionKeyText = T.pack $ symbolVal (Proxy @sessionKey)
      passwordText = T.pack $ symbolVal (Proxy @password)
      mockCurrentTime = mockNow

    unlockResult <-
      elements
        [ Left Agent.UnlockUnavailable
        , Left (Agent.UnlockFailed passwordText)
        , Right (Agent.SessionKey passwordText)
        ]
    listItemsResult <-
      elements
        [ Left Agent.ListItemsUnavailable
        , Left (Agent.ListItemsFailed sessionKeyText)
        , Right []
        ]
    syncResult <-
      elements
        [ Left Bitwarden.SyncUnavailable
        , Left (Bitwarden.SyncFailed sessionKeyText)
        , Right ()
        ]
    getPasswordResult <-
      elements
        [ Left Bitwarden.GetPasswordUnavailable
        , Left (Bitwarden.GetPasswordFailed sessionKeyText)
        , Right (Agent.PasswordValue passwordText)
        ]
    return $
      LeakingMockEnv $
        MockEnv {..}
