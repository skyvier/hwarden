{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hwarden.Logging
  ( LogMessage,
    logSafeInfo,
    sessionSanitizedLogMessage,
  )
where

import Data.String (IsString)
import Hwarden.Sanitize
  ( SanitizedText,
    Secret (SessionSecret, Static),
    getSanitizedText,
    trustStaticText,
  )
import Katip (KatipContext, Severity (InfoS), logStr, logTM)

newtype LogMessage = LogMessage (SanitizedText Static)
  deriving (IsString)

logSafeInfo :: KatipContext m => LogMessage -> m ()
logSafeInfo (LogMessage message) =
  $(logTM) InfoS (logStr (getSanitizedText message))

sessionSanitizedLogMessage :: SanitizedText SessionSecret -> LogMessage
sessionSanitizedLogMessage message =
  LogMessage (trustStaticText (getSanitizedText message))
