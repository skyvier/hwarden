{-# LANGUAGE TemplateHaskell #-}

module Hwarden.Logging
  ( logInfo,
  )
where

import Data.Text (Text)
import Katip (KatipContext, Severity (InfoS), logStr, logTM)

logInfo :: KatipContext m => Text -> m ()
logInfo message = $(logTM) InfoS (logStr message)
