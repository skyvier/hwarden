{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hwarden.Logging
  ( LogMessage,
    SafeLogger (..),
    ToLog (..),
    logInfoF,
    logSafeInfo,
    passwordSanitizedLogMessage,
    renderLogMessage,
    sessionSanitizedLogMessage,
  )
where

import Data.String (IsString)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Fcf (Exp, Eval)
import Hwarden.Sanitize
  ( SanitizedText,
    Secret (PasswordSecret, SessionSecret, Static),
    getSanitizedText,
    trustStaticText,
  )
import Hwarden.Types
  ( Password,
    PasswordValue,
    SessionKey
  )
import Katip (KatipContext, Severity (InfoS), logStr, logTM)
import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
    UnconsSymbol,
    symbolVal
  )

newtype LogMessage = LogMessage (SanitizedText Static)
  deriving (IsString)

renderLogMessage :: LogMessage -> Text
renderLogMessage (LogMessage message) =
  getSanitizedText message

logSafeInfo :: KatipContext m => LogMessage -> m ()
logSafeInfo message =
  $(logTM) InfoS (logStr (renderLogMessage message))

class Monad m => SafeLogger m where
  logInfoMessage :: LogMessage -> m ()
  logSessionSanitizedInfo :: SanitizedText SessionSecret -> m ()
  logPasswordSanitizedInfo :: SanitizedText PasswordSecret -> m ()

class ToLog a where
  toLogText :: a -> Text

instance ToLog SessionKey where
  toLogText _ = "[REDACTED]"

instance ToLog Password where
  toLogText _ = "[REDACTED]"

instance ToLog PasswordValue where
  toLogText _ = "[REDACTED]"

instance ToLog (SanitizedText secret) where
  toLogText = getSanitizedText

sessionSanitizedLogMessage :: SanitizedText SessionSecret -> LogMessage
sessionSanitizedLogMessage message =
  LogMessage (trustStaticText (getSanitizedText message))

passwordSanitizedLogMessage :: SanitizedText PasswordSecret -> LogMessage
passwordSanitizedLogMessage message =
  LogMessage (trustStaticText (getSanitizedText message))

logInfoF ::
  forall format m result.
  ( KnownSymbol format,
    SafeLogger m,
    BuildLogFunction (Eval (CountSlots format)) format m result
  ) =>
  result
logInfoF =
  buildLogFunction @(Eval (CountSlots format)) @format @m []

data SlotCount
  = NoSlots
  | OneMore SlotCount

data CountSlots :: Symbol -> Exp SlotCount
type instance Eval (CountSlots format) =
  CountSlotsFrom (UnconsSymbol format)

type family CountSlotsFrom (next :: Maybe (Char, Symbol)) :: SlotCount where
  CountSlotsFrom 'Nothing = 'NoSlots
  CountSlotsFrom ('Just '( '%', rest)) = CountSlotsAfterPercent (UnconsSymbol rest)
  CountSlotsFrom ('Just '(other, rest)) = Eval (CountSlots rest)

type family CountSlotsAfterPercent (next :: Maybe (Char, Symbol)) :: SlotCount where
  CountSlotsAfterPercent ('Just '( 's', rest)) = 'OneMore (Eval (CountSlots rest))
  CountSlotsAfterPercent 'Nothing = 'NoSlots
  CountSlotsAfterPercent ('Just '(other, rest)) = Eval (CountSlots rest)

class BuildLogFunction (slots :: SlotCount) (format :: Symbol) (m :: Type -> Type) result | result -> m where
  buildLogFunction ::
    (KnownSymbol format, SafeLogger m) =>
    [Text] ->
    result

instance BuildLogFunction 'NoSlots format m (m ()) where
  buildLogFunction values =
    logInfoMessage (formatLogMessage @format values)

instance
  (ToLog value, BuildLogFunction slots format m result) =>
  BuildLogFunction ('OneMore slots) format m (value -> result)
  where
  buildLogFunction values value =
    buildLogFunction @slots @format @m (values <> [toLogText value])

formatLogMessage :: forall format. KnownSymbol format => [Text] -> LogMessage
formatLogMessage values =
  LogMessage (trustStaticText (renderFormat (T.pack (symbolVal (Proxy @format))) values))

renderFormat :: Text -> [Text] -> Text
renderFormat format values =
  case T.uncons format of
    Nothing ->
      ""
    Just ('%', rest) ->
      case T.uncons rest of
        Just ('s', suffix) ->
          case values of
            value : remainingValues ->
              value <> renderFormat suffix remainingValues
            [] ->
              "%s" <> renderFormat suffix []
        Just (nextChar, suffix) ->
          T.cons '%' (T.cons nextChar (renderFormat suffix values))
        Nothing ->
          "%"
    Just (nextChar, rest) ->
      T.cons nextChar (renderFormat rest values)
