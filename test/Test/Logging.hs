{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Logging (tests) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import Hwarden.Logging
  ( SafeLogger (..),
    logInfoF,
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

instance SafeLogger CaptureLog where
  logInfoMessage message =
    CaptureLog (tell [renderLogMessage message])
  logSessionSanitizedInfo _ =
    CaptureLog (tell ["session-sanitized"])
  logPasswordSanitizedInfo _ =
    CaptureLog (tell ["password-sanitized"])

capturedLogs :: CaptureLog () -> [Text]
capturedLogs =
  execWriter . runCaptureLog

tests :: TestTree
tests =
  testGroup
    "typed logging"
    [ testCase "logInfoF logs a static message with no parameters" $
        capturedLogs (logInfoF @"startup complete")
          @?= ["startup complete"]
    , testCase "logInfoF formats a typed sanitized parameter" $
        capturedLogs
          ( logInfoF
              @"result: %{SessionSanitized}"
              (sanitizeListItemsFailure (Agent.SessionKey "raw-session-key") "safe error")
          )
          @?= ["result: safe error"]
    , testCase "logInfoF formats multiple typed parameters" $
        capturedLogs
          ( logInfoF
              @"session %{SessionKey} produced %{PasswordValue}"
              (Agent.SessionKey "raw-session-key")
              (Agent.PasswordValue "raw-password")
          )
          @?= ["session [REDACTED] produced [REDACTED]"]
    , testCase "logInfoF treats non-placeholder percent sequences as literals" $
        capturedLogs (logInfoF @"progress: 100%% complete")
          @?= ["progress: 100%% complete"]
    , testCase "logInfoF uses ToLog redaction for secrets" $
        capturedLogs
          (logInfoF @"session: %{SessionKey}" (Agent.SessionKey "raw-session-key"))
          @?= ["session: [REDACTED]"]
    ]
