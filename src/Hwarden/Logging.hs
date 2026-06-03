{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

-- | Typed logging helpers.
--
-- The API intentionally separates static log text from runtime values. Static
-- text can appear in a type-level string through 'logInfoS' or 'logInfoF', but
-- runtime values must be supplied through 'ToLog'. This makes accidental
-- logging of secrets harder: raw runtime 'Text' cannot be interpolated unless
-- the application explicitly provides a 'ToLog' instance for it.
--
-- Static messages use 'logInfoS':
--
-- @
-- logInfoS @"starting bitwarden sync"
-- @
--
-- Messages with runtime values use 'logInfoF':
--
-- @
-- logInfoF @"session %{SessionKey}" sessionKey
-- @
--
-- parses the format string at compile time, checks that each argument's
-- 'LogTypeName' matches the corresponding slot name, renders each value through
-- 'toLogText', and then sends the final 'LogMessage' through 'MonadLog'.
-- Application code should use 'logInfoS' or 'logInfoF'; direct use of
-- 'unsafeLogInfo' is only for concrete logging backends.
module Hwarden.Logging
  ( LogMessage,
    MonadLog (..),
    ToLog (..),
    Field,
    field,
    logInfoF,
    logInfoS,
    renderLogMessage,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import DeFun.Core (App, type (@@), type (~>))
import qualified GHC.TypeError as TE
import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
    symbolVal
  )
import GHC.Generics
  ( D,
    Generic (Rep),
    M1,
    Meta (MetaData)
  )
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
import Symparsec.Run (Run)

-- | Trusted log text ready to be sent to a logging backend.
--
-- The constructor is intentionally hidden. Use 'logInfoS' for static log
-- messages and 'logInfoF' for formatted log messages. There is deliberately no
-- 'Data.String.IsString' instance: even string literals should enter through a
-- type-level logging function.
newtype LogMessage = LogMessage Text

-- | Render a trusted log message.
--
-- This is mostly useful for tests and concrete logging backends.
renderLogMessage :: LogMessage -> Text
renderLogMessage (LogMessage message) =
  message

-- | A monad that can emit trusted informational log messages.
--
-- Define 'unsafeLogInfo' in concrete logging backends. Application code should
-- call 'logInfoS' or 'logInfoF' instead, so static text and runtime values pass
-- through the typed logging API before reaching the backend.
class Monad m => MonadLog m where
  unsafeLogInfo :: LogMessage -> m ()

-- | Values that may be safely interpolated into log messages.
--
-- The associated 'LogTypeName' is matched against typed slots in 'logInfoF'
-- format strings. 'LogTypeName' defaults to the type name from 'Generic'
-- metadata, but 'toLogText' has no default: rendering runtime data must remain
-- an explicit decision.
class ToLog a where
  -- | Type-level slot name accepted by this value.
  type LogTypeName a :: Symbol
  type LogTypeName a = DefaultLogTypeName a

  -- | Render a value into log-safe text.
  toLogText :: a -> Text

-- | Default log slot name derived from the type's 'Generic' metadata.
--
-- Types can use this default by deriving 'Generic' and omitting an explicit
-- 'LogTypeName'. Types whose log slot name should differ from their Haskell
-- type name can override the associated type.
type DefaultLogTypeName :: Type -> Symbol
type DefaultLogTypeName value =
  RepLogTypeName (Rep value)

type RepLogTypeName :: (Type -> Type) -> Symbol
type family RepLogTypeName rep where
  RepLogTypeName (M1 D ('MetaData name moduleName packageName isNewtype) fields) =
    name

-- | A value with a caller-chosen log slot name.
--
-- This lets a call site name a field without defining a new wrapper type. The
-- wrapped value still needs a 'ToLog' instance, so this does not grant logging
-- permission to arbitrary runtime values.
newtype Field (name :: Symbol) a = Field a

-- | Attach a caller-chosen type-level field name to a loggable value.
--
-- @
-- logInfoF @"value: %{chosen_identifier}" (field @"chosen_identifier" x)
-- @
field :: forall name a. a -> Field name a
field = Field

instance ToLog a => ToLog (Field name a) where
  type LogTypeName (Field name a) = name

  toLogText (Field value) = toLogText value

-- | Log an informational static message.
--
-- @
-- logInfoS @"starting bitwarden sync"
-- @
logInfoS ::
  forall message m.
  (KnownSymbol message, MonadLog m) =>
  m ()
logInfoS =
  unsafeLogInfo (LogMessage (T.pack (symbolVal (Proxy @message))))

-- | Log an informational message described by a type-level format string.
--
-- Use 'logInfoS' when the message has no runtime values.
--
-- Runtime values are supplied through typed slots:
--
-- @
-- logInfoF @"cache refresh failed: %{SessionSanitized}" err
-- @
--
-- The number of slots determines the number of arguments, and each argument's
-- 'LogTypeName' must match the slot name. Empty slots such as @%{}@ are
-- rejected at compile time.
logInfoF ::
  forall format m result.
  ( KnownSymbol format,
    MonadLog m,
    BuildLogFunction (ParseLogArguments format) format m result
  ) =>
  result
logInfoF =
  buildLogFunction @(ParseLogArguments format) @format @m []

-- | Parse a format string into the slot names it requires.
type ParseLogArguments :: Symbol -> [Symbol]
type ParseLogArguments format =
  ParsedLogArguments (Run LogFormatArguments format)

-- | Convert a symparsec result into a plain slot list or a type error.
--
-- The type-level pass only extracts typed slots. For example,
-- @"session %{SessionKey}"@ becomes @'["SessionKey"]@. All ordinary text is
-- skipped; runtime rendering below is responsible for preserving the message
-- text.
type ParsedLogArguments :: Either TE.ErrorMessage ([Symbol], Symbol) -> [Symbol]
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

-- | A parser that extracts typed slot names from a format string.
--
-- symparsec parsers are defunctionalized functions from parser state to reply.
type LogFormatArguments :: PParser [Symbol]
data LogFormatArguments s
type instance App LogFormatArguments s =
  LogFormatArgumentsLoop '[] s (UnconsState s)

-- | Scan the format text, collecting slot names in reverse order.
type LogFormatArgumentsLoop :: [Symbol] -> PState -> (Maybe Char, PState) -> PReply [Symbol]
type family LogFormatArgumentsLoop arguments previousState next where
  LogFormatArgumentsLoop arguments previousState '(Nothing, state) =
    'Reply ('OK (Reverse arguments)) previousState
  LogFormatArgumentsLoop arguments previousState '(Just '%', state) =
    LogFormatAfterPercent arguments state (UnconsState state)
  LogFormatArgumentsLoop arguments previousState '(Just other, state) =
    LogFormatArgumentsLoop arguments state (UnconsState state)

-- | Slot names continue until the closing brace.
--
-- Empty names are rejected later, after 'TakeWhile' returns the parsed slot
-- name and the parser checks the closing brace.
type IsLogSlotNameChar :: Char -> Bool
type family IsLogSlotNameChar char where
  IsLogSlotNameChar '}' = False
  IsLogSlotNameChar _ = True

type IsLogSlotNameCharSym :: Char ~> Bool
data IsLogSlotNameCharSym char
type instance App IsLogSlotNameCharSym char = IsLogSlotNameChar char

-- | Interpret the character after a percent sign.
type LogFormatAfterPercent :: [Symbol] -> PState -> (Maybe Char, PState) -> PReply [Symbol]
type family LogFormatAfterPercent arguments state next where
  LogFormatAfterPercent arguments state '(Just '{', slotStartState) =
    LogFormatSlotName arguments (TakeWhile IsLogSlotNameCharSym @@ slotStartState)
  LogFormatAfterPercent arguments state '(Just other, afterOtherState) =
    LogFormatArgumentsLoop arguments afterOtherState (UnconsState afterOtherState)
  LogFormatAfterPercent arguments state '(Nothing, endState) =
    'Reply ('OK (Reverse arguments)) state

-- | Continue after parsing the contents of a @%{...}@ slot.
type LogFormatSlotName :: [Symbol] -> PReply Symbol -> PReply [Symbol]
type family LogFormatSlotName arguments slotResult where
  LogFormatSlotName arguments ('Reply ('OK slotName) slotEndState) =
    LogFormatSlotClose arguments slotName slotEndState (UnconsState slotEndState)
  LogFormatSlotName arguments ('Reply ('Err err) state) =
    'Reply ('Err err) state

-- | Require a closing brace after a slot name.
--
-- A closing brace immediately after the opening brace is an empty slot and
-- produces a type-level parse error.
type LogFormatSlotClose :: [Symbol] -> Symbol -> PState -> (Maybe Char, PState) -> PReply [Symbol]
type family LogFormatSlotClose arguments slotName state next where
  LogFormatSlotClose arguments "" state '(Just '}', afterCloseState) =
    'Reply ('Err (Error1 "empty log format slot")) state
  LogFormatSlotClose arguments slotName state '(Just '}', afterCloseState) =
    LogFormatArgumentsLoop
      (slotName ': arguments)
      afterCloseState
      (UnconsState afterCloseState)
  LogFormatSlotClose arguments slotName state '(Just other, afterOtherState) =
    'Reply ('Err (Error1 "expected closing '}' in log format slot")) state
  LogFormatSlotClose arguments slotName state '(Nothing, endState) =
    'Reply ('Err (Error1 "unterminated log format slot")) state

-- | Restore left-to-right slot order after reverse accumulation.
type Reverse :: [Symbol] -> [Symbol]
type Reverse values =
  ReverseOnto values '[]

type ReverseOnto :: [Symbol] -> [Symbol] -> [Symbol]
type family ReverseOnto values accumulator where
  ReverseOnto '[] accumulator = accumulator
  ReverseOnto (value ': values) accumulator =
    ReverseOnto values (value ': accumulator)

-- | Build the curried 'logInfoF' result type from the parsed slot list.
--
-- The slot list determines arity. The actual argument types are inferred from
-- the values passed by the caller, then checked with
-- @LogTypeName argument ~ slotName@.
class BuildLogFunction (slots :: [Symbol]) (format :: Symbol) (m :: Type -> Type) result | result -> m where
  buildLogFunction ::
    (KnownSymbol format, MonadLog m) =>
    [Text] ->
    result

instance BuildLogFunction '[] format m (m ()) where
  buildLogFunction values =
    unsafeLogInfo (formatLogMessage @format values)

instance
  ( ToLog argument,
    LogTypeName argument ~ slotName,
    BuildLogFunction slotNames format m result
  ) =>
  BuildLogFunction (slotName ': slotNames) format m (argument -> result)
  where
  buildLogFunction values value =
    buildLogFunction @slotNames @format @m (values <> [toLogText value])

-- | Render a format string and already-safe slot values into a 'LogMessage'.
formatLogMessage :: forall format. KnownSymbol format => [Text] -> LogMessage
formatLogMessage values =
  LogMessage (renderFormat (T.pack (symbolVal (Proxy @format))) values)

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
