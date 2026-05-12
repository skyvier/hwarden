module Hwarden.Types
  ( Password (..),
    SessionKey (..),
    Username (..)
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary (arbitrary))

newtype SessionKey = SessionKey Text
  deriving (Eq)

instance Show SessionKey where
  show _ = "[REDACTED]"

instance Arbitrary SessionKey where
  arbitrary = SessionKey . T.pack <$> arbitrary

newtype Username = Username Text
  deriving (Eq, Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"
