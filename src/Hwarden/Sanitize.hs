{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Hwarden.Sanitize (
  SanitizedText,
  getSanitizedText,
  Secret (..),
  trustStaticText,
  sanitizeUnlockError,
  sanitizeGetPasswordFailure,
  sanitizeListItemsFailure,
  sanitizeSyncFailure,
)
where

import Data.String (IsString (fromString))
import Data.Text
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hwarden.Logging (ToLog (..))
import Hwarden.Types (Password (..), SessionKey (..))
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

data Secret
  = Static
  | PasswordSecret
  | SessionSecret

newtype SanitizedText (secretType :: Secret)
  = SanitizedText {getSanitizedText :: Text}
  deriving stock (Generic)
  deriving newtype (Eq, Show)

type role SanitizedText nominal

instance IsString (SanitizedText Static) where
  fromString = SanitizedText . T.pack

instance ToLog (SanitizedText Static) where
  type LogTypeName (SanitizedText Static) = "StaticSanitized"

  toLogText = getSanitizedText

instance ToLog (SanitizedText PasswordSecret) where
  type LogTypeName (SanitizedText PasswordSecret) = "PasswordSanitized"

  toLogText = getSanitizedText

instance ToLog (SanitizedText SessionSecret) where
  type LogTypeName (SanitizedText SessionSecret) = "SessionSanitized"

  toLogText = getSanitizedText

trustStaticText :: Text -> SanitizedText Static
trustStaticText = SanitizedText

instance Arbitrary (SanitizedText Static) where
  arbitrary = SanitizedText . T.pack <$> arbitrary
  shrink = genericShrink

instance Arbitrary (SanitizedText PasswordSecret) where
  arbitrary = mkPasswordSanitized . T.pack <$> arbitrary
  shrink = genericShrink

mkPasswordSanitized :: Text -> SanitizedText PasswordSecret
mkPasswordSanitized = SanitizedText

instance Arbitrary (SanitizedText SessionSecret) where
  arbitrary = mkSessionSanitized . T.pack <$> arbitrary
  shrink = genericShrink

mkSessionSanitized :: Text -> SanitizedText SessionSecret
mkSessionSanitized = SanitizedText

sanitizeUnlockError :: Password -> Text -> SanitizedText PasswordSecret
sanitizeUnlockError (Password password) err =
  let
    sanitized =
      if T.null password
        then err
        else T.replace password "<redacted>" err
    trimmed = T.strip sanitized
   in
    if T.null trimmed
      then SanitizedText "bw login failed"
      else SanitizedText trimmed

sanitizeListItemsFailure :: SessionKey -> Text -> SanitizedText SessionSecret
sanitizeListItemsFailure =
  sanitizeSessionKey "bw list items failed"

sanitizeSyncFailure :: SessionKey -> Text -> SanitizedText SessionSecret
sanitizeSyncFailure =
  sanitizeSessionKey "bw sync failed"

sanitizeGetPasswordFailure :: SessionKey -> Text -> SanitizedText SessionSecret
sanitizeGetPasswordFailure =
  sanitizeSessionKey "bw get password failed"

-- Fallbacks must be static non-secret text.
sanitizeSessionKey :: Text -> SessionKey -> Text -> SanitizedText SessionSecret
sanitizeSessionKey fallback (SessionKey sessionKey) err =
  let
    sanitized =
      if T.null sessionKey
        then err
        else T.replace sessionKey "<redacted>" err
    trimmed = T.strip sanitized
   in
    if T.null trimmed
      then SanitizedText fallback
      else SanitizedText trimmed
