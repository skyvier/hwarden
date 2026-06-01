{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Logging (tests) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import Hwarden.Logging
  ( SafeLogger (..),
    ToLog (..),
    logInfoF,
    renderLogMessage
  )
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

newtype PublicText = PublicText Text

instance ToLog PublicText where
  toLogText (PublicText value) = value

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
    , testCase "logInfoF formats one loggable parameter" $
        capturedLogs (logInfoF @"result: %s" (PublicText "ok"))
          @?= ["result: ok"]
    , testCase "logInfoF formats multiple loggable parameters" $
        capturedLogs
          ( logInfoF
              @"request %s returned %s"
              (PublicText "status")
              (PublicText "unlocked")
          )
          @?= ["request status returned unlocked"]
    , testCase "logInfoF treats non-placeholder percent sequences as literals" $
        capturedLogs (logInfoF @"progress: 100%% complete")
          @?= ["progress: 100%% complete"]
    , testCase "logInfoF uses ToLog redaction for secrets" $
        capturedLogs
          (logInfoF @"session: %s" (Agent.SessionKey "raw-session-key"))
          @?= ["session: [REDACTED]"]
    ]
