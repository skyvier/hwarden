{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Logging (tests) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hwarden.Logging
  ( MonadLog (..),
    ToLog (..),
    field,
    logInfoF,
    logInfoS,
    renderLogMessage
  )
import Hwarden.Sanitize (sanitizeListItemsFailure)
import qualified Hwarden.Agent as Agent
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype CaptureLog a = CaptureLog
  { runCaptureLog :: Writer [Text] a
  }
  deriving (Functor, Applicative, Monad)

instance MonadLog CaptureLog where
  logInfo message =
    CaptureLog (tell [renderLogMessage message])

newtype PublicLogValue = PublicLogValue Text
  deriving (Generic)

instance ToLog PublicLogValue where
  toLogText (PublicLogValue value) = value

capturedLogs :: CaptureLog () -> [Text]
capturedLogs =
  execWriter . runCaptureLog

tests :: TestTree
tests =
  testGroup
    "typed logging"
    [ testCase "logInfoS logs a static message" $
        capturedLogs (logInfoS @"startup complete")
          @?= ["startup complete"]
    , testCase "logInfoF formats a typed sanitized parameter" $
        capturedLogs
          ( logInfoF
              @"result: %{SessionSanitized}"
              (sanitizeListItemsFailure (Agent.SessionKey "raw-session-key") "safe error")
          )
          @?= ["result: safe error"]
    , testCase "logInfoF accepts slot names from ToLog instances" $
        capturedLogs
          (logInfoF @"result: %{PublicLogValue}" (PublicLogValue "ok"))
          @?= ["result: ok"]
    , testCase "logInfoF accepts caller-chosen field names" $
        capturedLogs
          ( logInfoF
              @"this is a value: %{chosen_identifier}"
              (field @"chosen_identifier" (PublicLogValue "ok"))
          )
          @?= ["this is a value: ok"]
    , testCase "logInfoF formats multiple typed parameters" $
        capturedLogs
          ( logInfoF
              @"session %{SessionKey} produced %{PasswordValue}"
              (Agent.SessionKey "raw-session-key")
              (Agent.PasswordValue "raw-password")
          )
          @?= ["session [REDACTED] produced [REDACTED]"]
    , testCase "logInfoS treats percent sequences as static text" $
        capturedLogs (logInfoS @"progress: 100%% complete")
          @?= ["progress: 100%% complete"]
    , testCase "logInfoF uses ToLog redaction for secrets" $
        capturedLogs
          (logInfoF @"session: %{SessionKey}" (Agent.SessionKey "raw-session-key"))
          @?= ["session: [REDACTED]"]
    ]
