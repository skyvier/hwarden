module Hwarden.Types
  ( Password (..),
    SessionKey (..),
    Username (..)
  )
where

import Data.Text (Text)

newtype SessionKey = SessionKey Text
  deriving (Eq)

instance Show SessionKey where
  show _ = "[REDACTED]"

newtype Username = Username Text
  deriving (Eq, Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"
