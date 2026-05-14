{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hwarden.App
  ( AgentT (..),
    Env (..),
    initAgentLogEnv,
    runAgentT,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Hwarden.Bitwarden (Bitwarden)
import Hwarden.Bitwarden.Real (RealBitwardenT (..))
import Katip
  ( ColorStrategy (ColorIfTerminal),
    Katip,
    KatipContext,
    LogEnv,
    LogContexts,
    Namespace,
    Severity (..),
    Verbosity (V2),
    defaultScribeSettings,
    getKatipContext,
    getKatipNamespace,
    getLogEnv,
    initLogEnv,
    localKatipContext,
    localKatipNamespace,
    localLogEnv,
    mkHandleScribe,
    permitItem,
    registerScribe
  )
import System.IO (stdout)
import UnliftIO (MonadUnliftIO)

data Env = Env
  { envLogEnv :: LogEnv,
    envLogContexts :: LogContexts,
    envNamespace :: Namespace
  }

newtype AgentT a = AgentT
  { runAgentTInternal :: ReaderT Env IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)
  deriving (Bitwarden) via (RealBitwardenT AgentT)

instance Katip AgentT where
  getLogEnv = asks envLogEnv
  localLogEnv f = AgentT . local updateLogEnv . runAgentTInternal
    where
      updateLogEnv env = env {envLogEnv = f (envLogEnv env)}

instance KatipContext AgentT where
  getKatipContext = asks envLogContexts
  localKatipContext f = AgentT . local updateLogContexts . runAgentTInternal
    where
      updateLogContexts env = env {envLogContexts = f (envLogContexts env)}
  getKatipNamespace = asks envNamespace
  localKatipNamespace f = AgentT . local updateNamespace . runAgentTInternal
    where
      updateNamespace env = env {envNamespace = f (envNamespace env)}

runAgentT :: Env -> AgentT a -> IO a
runAgentT env =
  flip runReaderT env . runAgentTInternal

initAgentLogEnv :: IO LogEnv
initAgentLogEnv = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
  baseLogEnv <- initLogEnv "hwarden-agent" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings baseLogEnv
