{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleInstances #-}

module Hwarden.Sanitize 
  ( SanitizedText
  , getSanitizedText
  , Secret(..)
  , trustStaticText
  , sanitizeUnlockError
  , sanitizeGetPasswordFailure
  , sanitizeListItemsFailure
  , sanitizeSyncFailure
  )
  where

import Data.Text
import qualified Data.Text as T
import Hwarden.Types (Password (..), SessionKey (..))
import Test.QuickCheck
import Data.String (IsString (fromString))

data Secret 
  = Static
  | PasswordSecret
  | SessionSecret

newtype SanitizedText (secretType :: Secret) 
  = SanitizedText { getSanitizedText :: Text }
  deriving newtype (Eq, Show)

type role SanitizedText nominal

instance IsString (SanitizedText Static) where
  fromString = SanitizedText . T.pack

trustStaticText :: Text -> SanitizedText Static
trustStaticText = SanitizedText

instance Arbitrary (SanitizedText PasswordSecret) where
  arbitrary = mkPasswordSanitized . T.pack <$> arbitrary

mkPasswordSanitized :: Text -> SanitizedText PasswordSecret
mkPasswordSanitized = SanitizedText 

instance Arbitrary (SanitizedText SessionSecret) where
  arbitrary = mkSessionSanitized . T.pack <$> arbitrary

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

-- XXX: User is responsible for not passing secrets in fallback
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
