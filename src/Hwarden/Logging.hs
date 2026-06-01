{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hwarden.Logging
  ( LogMessage,
    SafeLogger (..),
    logSafeInfo,
    passwordSanitizedLogMessage,
    sessionSanitizedLogMessage,
  )
where

import Data.String (IsString)
import Hwarden.Sanitize
  ( SanitizedText,
    Secret (PasswordSecret, SessionSecret, Static),
    getSanitizedText,
    trustStaticText,
  )
import Katip (KatipContext, Severity (InfoS), logStr, logTM)

newtype LogMessage = LogMessage (SanitizedText Static)
  deriving (IsString)

logSafeInfo :: KatipContext m => LogMessage -> m ()
logSafeInfo (LogMessage message) =
  $(logTM) InfoS (logStr (getSanitizedText message))

class Monad m => SafeLogger m where
  logInfoMessage :: LogMessage -> m ()
  logSessionSanitizedInfo :: SanitizedText SessionSecret -> m ()
  logPasswordSanitizedInfo :: SanitizedText PasswordSecret -> m ()

sessionSanitizedLogMessage :: SanitizedText SessionSecret -> LogMessage
sessionSanitizedLogMessage message =
  LogMessage (trustStaticText (getSanitizedText message))

passwordSanitizedLogMessage :: SanitizedText PasswordSecret -> LogMessage
passwordSanitizedLogMessage message =
  LogMessage (trustStaticText (getSanitizedText message))
