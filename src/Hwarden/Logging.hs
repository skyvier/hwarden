{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
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

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import DeFun.Core (App, type (@@))
import qualified GHC.TypeError as TE
import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
    symbolVal
  )
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
import Symparsec.Parser.Common
  ( PParser,
    PReply,
    PState,
    Reply (..),
    Result (..),
    UnconsState,
    type Error1
  )
import Symparsec.Parser.TakeWhile (TakeWhile)
import Symparsec.Parser.While.Predicates (IsAlphaSym)
import Symparsec.Run (Run)

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
  forall format m.
  ( KnownSymbol format,
    SafeLogger m,
    BuildLogFunction (ParseLogArguments format) format m
  ) =>
  LogFunction (ParseLogArguments format) format m
logInfoF =
  buildLogFunction @(ParseLogArguments format) @format @m []

type ParseLogArguments :: Symbol -> [Type]
type ParseLogArguments format =
  ParsedLogArguments (Run LogFormatArguments format)

-- The type-level pass only extracts typed slots. For example,
-- "session %{SessionKey}" becomes '[SessionKey]. All ordinary text is skipped;
-- runtime rendering below is responsible for preserving the message text.
type ParsedLogArguments :: Either TE.ErrorMessage ([Type], Symbol) -> [Type]
type family ParsedLogArguments parseResult where
  ParsedLogArguments (Right '(arguments, "")) =
    arguments
  ParsedLogArguments (Right '(arguments, remaining)) =
    TE.TypeError
      ( TE.Text "Unexpected unparsed log format suffix: "
          TE.:<>: TE.ShowType remaining
      )
  ParsedLogArguments (Left err) =
    TE.TypeError err

type LogFormatArguments :: PParser [Type]
data LogFormatArguments s
type instance App LogFormatArguments s =
  LogFormatArgumentsLoop '[] s (UnconsState s)

type LogFormatArgumentsLoop :: [Type] -> PState -> (Maybe Char, PState) -> PReply [Type]
type family LogFormatArgumentsLoop arguments previousState next where
  LogFormatArgumentsLoop arguments previousState '(Nothing, state) =
    'Reply ('OK (Reverse arguments)) previousState
  LogFormatArgumentsLoop arguments previousState '(Just '%', state) =
    LogFormatAfterPercent arguments state (UnconsState state)
  LogFormatArgumentsLoop arguments previousState '(Just other, state) =
    LogFormatArgumentsLoop arguments state (UnconsState state)

type LogFormatAfterPercent :: [Type] -> PState -> (Maybe Char, PState) -> PReply [Type]
type family LogFormatAfterPercent arguments state next where
  LogFormatAfterPercent arguments state '(Just '{', slotStartState) =
    LogFormatSlotName arguments (TakeWhile IsAlphaSym @@ slotStartState)
  LogFormatAfterPercent arguments state '(Just other, afterOtherState) =
    LogFormatArgumentsLoop arguments afterOtherState (UnconsState afterOtherState)
  LogFormatAfterPercent arguments state '(Nothing, endState) =
    'Reply ('OK (Reverse arguments)) state

type LogFormatSlotName :: [Type] -> PReply Symbol -> PReply [Type]
type family LogFormatSlotName arguments slotResult where
  LogFormatSlotName arguments ('Reply ('OK slotName) slotEndState) =
    LogFormatSlotClose arguments slotName slotEndState (UnconsState slotEndState)
  LogFormatSlotName arguments ('Reply ('Err err) state) =
    'Reply ('Err err) state

type LogFormatSlotClose :: [Type] -> Symbol -> PState -> (Maybe Char, PState) -> PReply [Type]
type family LogFormatSlotClose arguments slotName state next where
  LogFormatSlotClose arguments slotName state '(Just '}', afterCloseState) =
    LogFormatArgumentsLoop
      (LogType slotName ': arguments)
      afterCloseState
      (UnconsState afterCloseState)
  LogFormatSlotClose arguments slotName state '(Just other, afterOtherState) =
    'Reply ('Err (Error1 "expected closing '}' in log format slot")) state
  LogFormatSlotClose arguments slotName state '(Nothing, endState) =
    'Reply ('Err (Error1 "unterminated log format slot")) state

type family LogType (name :: Symbol) :: Type where
  LogType "SessionKey" = SessionKey
  LogType "Password" = Password
  LogType "PasswordValue" = PasswordValue
  LogType "SessionSanitized" = SanitizedText SessionSecret
  LogType "PasswordSanitized" = SanitizedText PasswordSecret
  LogType name =
    TE.TypeError
      ( TE.Text "Unsupported log slot type: "
          TE.:<>: TE.ShowType name
      )

type Reverse :: [Type] -> [Type]
type Reverse values =
  ReverseOnto values '[]

type ReverseOnto :: [Type] -> [Type] -> [Type]
type family ReverseOnto values accumulator where
  ReverseOnto '[] accumulator = accumulator
  ReverseOnto (value ': values) accumulator =
    ReverseOnto values (value ': accumulator)

class BuildLogFunction (arguments :: [Type]) (format :: Symbol) (m :: Type -> Type) where
  type LogFunction arguments format m :: Type

  buildLogFunction ::
    (KnownSymbol format, SafeLogger m) =>
    [Text] ->
    LogFunction arguments format m

instance BuildLogFunction '[] format m where
  type LogFunction '[] format m = m ()

  buildLogFunction values =
    logInfoMessage (formatLogMessage @format values)

instance
  (ToLog argument, BuildLogFunction arguments format m) =>
  BuildLogFunction (argument ': arguments) format m
  where
  type LogFunction (argument ': arguments) format m =
    argument -> LogFunction arguments format m

  buildLogFunction values value =
    buildLogFunction @arguments @format @m (values <> [toLogText value])

formatLogMessage :: forall format. KnownSymbol format => [Text] -> LogMessage
formatLogMessage values =
  LogMessage (trustStaticText (renderFormat (T.pack (symbolVal (Proxy @format))) values))

-- The runtime pass substitutes typed slots in order. The type-level pass has
-- already fixed the arity and argument types, so this code only needs to walk
-- the format text and splice in the pre-sanitized ToLog renderings.
renderFormat :: Text -> [Text] -> Text
renderFormat format values =
  case T.uncons format of
    Nothing ->
      ""
    Just ('%', rest) ->
      case T.uncons rest of
        Just ('{', suffix) ->
          renderSlot suffix values
        Just (nextChar, suffix) ->
          T.cons '%' (T.cons nextChar (renderFormat suffix values))
        Nothing ->
          "%"
    Just (nextChar, rest) ->
      T.cons nextChar (renderFormat rest values)

renderSlot :: Text -> [Text] -> Text
renderSlot format values =
  let (_slotName, afterSlotName) = T.breakOn "}" format
   in case T.uncons afterSlotName of
        Just ('}', suffix) ->
          case values of
            value : remainingValues ->
              value <> renderFormat suffix remainingValues
            [] ->
              "%{" <> format
        _ ->
          "%{" <> renderFormat format values
