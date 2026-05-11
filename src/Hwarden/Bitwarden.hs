{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Bitwarden
  ( Bitwarden (..),
    UnlockError (..)
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Types (Password (Password), SessionKey (SessionKey), Username (Username))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

data UnlockError
  = UnlockUnavailable
  | UnlockFailed Text
  deriving (Eq, Show)

class Monad m => Bitwarden m where
  unlock :: Username -> Password -> m (Either UnlockError SessionKey)

instance Bitwarden IO where
  unlock (Username email) (Password password) = do
    let args = [T.unpack email, T.unpack password, "--raw"]
    result <-
      try (readProcessWithExitCode "bw" ("login" : args) "") ::
        IO (Either SomeException (ExitCode, String, String))
    pure $
      case result of
        Left _ ->
          Left UnlockUnavailable
        Right (exitCode, stdoutText, stderrText) ->
          case exitCode of
            ExitSuccess ->
              Right (SessionKey (T.strip (T.pack stdoutText)))
            ExitFailure _ ->
              Left (UnlockFailed (T.pack stderrText))
