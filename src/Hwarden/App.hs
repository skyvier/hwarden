{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hwarden.App
  ( AgentT (..),
    Env (..),
    initAgentEnv,
    runAgentT,
  )
where

import Control.Monad.Time (MonadTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Data.Text (Text)
import Hwarden.Bitwarden (Bitwarden, determineBitwardenServerUrl)
import Hwarden.Bitwarden.Real
  ( HasBitwardenCliConfig (..),
    RealBitwardenT (..)
  )
import qualified Hwarden.Runtime as Runtime
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
import System.Environment (lookupEnv)
import System.IO (stdout)
import Text.Read (readMaybe)
import qualified Control.Monad.Time as MonadTime
import UnliftIO (MonadUnliftIO)

data Env = Env
  { envLogEnv :: LogEnv,
    envLogContexts :: LogContexts,
    envNamespace :: Namespace,
    envBitwardenCliAppDataDir :: FilePath,
    envBitwardenServerUrl :: Text,
    envCacheRefreshIntervalSeconds :: Int
  }

newtype AgentT a = AgentT
  { runAgentTInternal :: ReaderT Env IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)
  deriving (Bitwarden) via (RealBitwardenT Env AgentT)

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

instance MonadTime AgentT where
  currentTime = liftIO MonadTime.currentTime
  monotonicTime = liftIO MonadTime.monotonicTime

instance HasBitwardenCliConfig Env where
  bitwardenCliAppDataDir = envBitwardenCliAppDataDir
  bitwardenServerUrl = envBitwardenServerUrl

runAgentT :: Env -> AgentT a -> IO a
runAgentT env =
  flip runReaderT env . runAgentTInternal

initAgentEnv :: FilePath -> IO Env
initAgentEnv runtimeDir = do
  let isolatedBitwardenCliAppDataDir =
        Runtime.bitwardenCliAppDataDir (Runtime.deriveAgentPaths runtimeDir)
  serverUrl <- lookupEnv "HWARDEN_SERVER_URL"
  cacheRefreshIntervalSeconds <-
    maybe defaultCacheRefreshIntervalSeconds parseCacheRefreshIntervalSeconds
      <$> lookupEnv "HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS"
  logEnv <- initAgentLogEnv
  return $
    Env
      logEnv
      mempty
      "hwarden-agent"
      isolatedBitwardenCliAppDataDir
      (determineBitwardenServerUrl serverUrl)
      cacheRefreshIntervalSeconds

initAgentLogEnv :: IO LogEnv
initAgentLogEnv = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
  baseLogEnv <- initLogEnv "hwarden-agent" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings baseLogEnv

defaultCacheRefreshIntervalSeconds :: Int
defaultCacheRefreshIntervalSeconds = 60

parseCacheRefreshIntervalSeconds :: String -> Int
parseCacheRefreshIntervalSeconds value =
  case readMaybe value of
    Just intervalSeconds | intervalSeconds > 0 -> intervalSeconds
    _ -> defaultCacheRefreshIntervalSeconds
